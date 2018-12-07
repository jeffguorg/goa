package codegen

import (
	"bytes"
	"fmt"
	"strings"
	"text/template"

	"goa.design/goa/expr"
)

var (
	transformArrayT *template.Template
	transformMapT   *template.Template
)

type (
	// Transformer transforms a source attribute to a target attribute.
	Transformer interface {
		// SourceVar returns the source variable used in the transformation code.
		// If non-empty field param is given, it suffixes the variable name with
		// the field name.
		SourceVar(field string) string
		// TargetVar returns the target variable used in the transformation code.
		// If non-empty field param is given, it suffixes the variable name with
		// the field name.
		TargetVar(field string) string
		// Assign returns the assignment operator to initialize target from source.
		Assign() string
		// MakeCompatible checks if source and target attributes are compatible
		// for transformation. It returns the compatible source and target
		// attribute or an error if source cannot be transformed to target.
		MakeCompatible(source, target AttributeAnalyzer, suffix string) (src, tgt AttributeAnalyzer, err error)
		// HelperName returns the name for the transform function to transform
		// source to the target attribute.
		HelperName(source, target AttributeAnalyzer) string
		// ConvertType adds type conversion code (if any) against varn based on
		// the attribute type.
		ConvertType(varn string, typ expr.DataType) (string, bool)
		// Dup creates a copy of the transformer updating the source and target
		// variables.
		Dup(sourceVar, targetVar string, newVar bool) Transformer
	}

	// TransformAttrs defines the fields to transform a source attribute to a
	// target attribute. It implements the Transformer interface.
	TransformAttrs struct {
		// SourceVarn and TargetVarn are the source and target variable names used
		// in the transformation code.
		SourceVarn, TargetVarn string
		// NewVar if true indicates that the the target must be initialized using
		// ":=". If false, target is initialized with "=".
		NewVar bool
		// HelperPrefix is the prefix for the helper functions generated during
		// the transformation. The helper functions are named based on this
		// pattern - <HelperPrefix><SourceTypeName>To<TargetTypeName>. If no prefix
		// specified, "transform" is used as a prefix by default.
		HelperPrefix string
	}

	// TransformFunctionData describes a helper function used to transform
	// user types. These are necessary to prevent potential infinite
	// recursion when a type attribute is defined recursively. For example:
	//
	//     var Recursive = Type("Recursive", func() {
	//         Attribute("r", "Recursive")
	//     }
	//
	// Transforming this type requires generating an intermediary function:
	//
	//     func recursiveToRecursive(r *Recursive) *service.Recursive {
	//         var t service.Recursive
	//         if r.R != nil {
	//             t.R = recursiveToRecursive(r.R)
	//         }
	//    }
	//
	TransformFunctionData struct {
		Name          string
		ParamTypeRef  string
		ResultTypeRef string
		Code          string
	}
)

// NOTE: can't initialize inline because https://github.com/golang/go/issues/1817
func init() {
	funcMap := template.FuncMap{"transformAttribute": transformAttributeHelper}
	transformArrayT = template.Must(template.New("transformArray").Funcs(funcMap).Parse(transformArrayTmpl))
	transformMapT = template.Must(template.New("transformMap").Funcs(funcMap).Parse(transformMapTmpl))
}

// Transform produces Go code that initializes the data structure defined
// by target from an instance of the data structure described by source.
// The data structures can be objects, arrays or maps. The algorithm
// matches object fields by name and ignores object fields in target that
// don't have a match in source. The matching and generated code leverage
// mapped attributes so that attribute names may use the "name:elem"
// syntax to define the name of the design attribute and the name of the
// corresponding generated Go struct field. The function returns an error
// if target is not compatible with source (different type, fields of
// different type etc).
//
// source and target are the attributes used in the transformation
//
// sourceVar and targetVar are the variable names used in the transformation
//
// prefix is the prefix added to transformation function helpers
//
func Transform(source, target AttributeAnalyzer, sourceVar, targetVar, prefix string) (string, []*TransformFunctionData, error) {
	t := &TransformAttrs{
		SourceVarn:   sourceVar,
		TargetVarn:   targetVar,
		HelperPrefix: prefix,
		NewVar:       true,
	}

	return TransformWith(source, target, t)
}

// TransformWith calls the Transform function with the given Transformer.
func TransformWith(source, target AttributeAnalyzer, t Transformer) (string, []*TransformFunctionData, error) {
	code, err := transformAttribute(source, target, t)
	if err != nil {
		return "", nil, err
	}

	funcs, err := transformAttributeHelpers(source, target, t)
	if err != nil {
		return "", nil, err
	}

	return strings.TrimRight(code, "\n"), funcs, nil
}

// AppendHelpers takes care of only appending helper functions from newH that
// are not already in oldH.
func AppendHelpers(oldH, newH []*TransformFunctionData) []*TransformFunctionData {
	for _, h := range newH {
		found := false
		for _, h2 := range oldH {
			if h.Name == h2.Name {
				found = true
				break
			}
		}
		if !found {
			oldH = append(oldH, h)
		}
	}
	return oldH
}

// MakeCompatible checks if source and target attributes are compatible
// for transformation. It returns the compatible source and target
// attribute or an error if source cannot be transformed to target.
func (t *TransformAttrs) MakeCompatible(source, target AttributeAnalyzer, suffix string) (AttributeAnalyzer, AttributeAnalyzer, error) {
	sourceType := source.Attribute().Type
	targetType := target.Attribute().Type
	if err := isCompatible(sourceType, targetType, t.SourceVarn+suffix, t.TargetVarn+suffix); err != nil {
		return source, target, err
	}
	return source, target, nil
}

// HelperName returns the name for the transform function.
func (t *TransformAttrs) HelperName(source, target AttributeAnalyzer) string {
	var (
		sname  string
		tname  string
		prefix string
	)
	{
		sname = Goify(source.Name(true), true)
		tname = Goify(target.Name(true), true)
		prefix = t.HelperPrefix
		if prefix == "" {
			prefix = "transform"
		}
	}
	return Goify(prefix+sname+"To"+tname, false)
}

// ConvertType converts varn to type typ.
func (t *TransformAttrs) ConvertType(varn string, typ expr.DataType) (string, bool) {
	return varn, false
}

// Assign returns the string to initialze the target variable. If NewVar is
// true it returns ":=", else "=".
func (t *TransformAttrs) Assign() string {
	if t.NewVar {
		return ":="
	}
	return "="
}

// SourceVar returns the source variable used in the transformation code.
// If non-empty field param is given, it suffixes the variable name with
// the field name.
func (t *TransformAttrs) SourceVar(field string) string {
	s := t.SourceVarn
	if field != "" {
		s += "." + field
	}
	return s
}

// TargetVar returns the target variable used in the transformation code.
// If non-empty field param is given, it suffixes the variable name with
// the field name.
func (t *TransformAttrs) TargetVar(field string) string {
	s := t.TargetVarn
	if field != "" {
		s += "." + field
	}
	return s
}

// Dup creates a copy of the transformer updating the source and target
// variables.
func (t *TransformAttrs) Dup(sourceVar, targetVar string, newVar bool) Transformer {
	return &TransformAttrs{
		SourceVarn:   sourceVar,
		TargetVarn:   targetVar,
		HelperPrefix: t.HelperPrefix,
		NewVar:       newVar,
	}
}

// transformAttribute transforms source attribute to target attribute.
func transformAttribute(source, target AttributeAnalyzer, t Transformer) (string, error) {
	var err error
	if source, target, err = t.MakeCompatible(source, target, ""); err != nil {
		return "", err
	}

	var (
		code string

		sourceType = source.Attribute().Type
		targetType = target.Attribute().Type
	)
	switch {
	case expr.IsArray(sourceType):
		code, err = transformArray(expr.AsArray(sourceType), expr.AsArray(targetType), source, target, t)
	case expr.IsMap(sourceType):
		code, err = transformMap(expr.AsMap(sourceType), expr.AsMap(targetType), source, target, t)
	case expr.IsObject(sourceType):
		code, err = transformObject(source, target, t)
	default:
		if _, ok := targetType.(expr.UserType); ok {
			// Primitive user type, these are used for error results
			cast := target.Ref(true)
			return fmt.Sprintf("%s %s %s(%s)\n", t.TargetVar(""), t.Assign(), cast, t.SourceVar("")), nil
		}
		srcField, _ := t.ConvertType(t.SourceVar(""), sourceType)
		code = fmt.Sprintf("%s %s %s\n", t.TargetVar(""), t.Assign(), srcField)
	}
	if err != nil {
		return "", err
	}
	return code, nil
}

// transformObject transfroms source object type to target object type.
func transformObject(source, target AttributeAnalyzer, t Transformer) (string, error) {
	var (
		initCode     string
		postInitCode string
	)
	{
		// iterate through primitive attributes to initialize the struct
		walkMatches(source, target, func(srcMatt, tgtMatt *expr.MappedAttributeExpr, srcc, tgtc AttributeAnalyzer, n string) {
			if !expr.IsPrimitive(srcc.Attribute().Type) {
				return
			}
			srcField := t.SourceVar(srcc.Identifier(srcMatt.ElemName(n), true))
			tgtField := tgtc.Identifier(tgtMatt.ElemName(n), true)
			srcPtr := srcc.IsPointer()
			tgtPtr := tgtc.IsPointer()

			deref := ""
			if srcPtr && !tgtPtr {
				deref = "*"
				if !srcc.IsRequired() {
					srcFieldConv, _ := t.ConvertType("*"+srcField, srcc.Attribute().Type)
					postInitCode += fmt.Sprintf("if %s != nil {\n\t%s = %s\n}\n", srcField, t.TargetVar(tgtField), srcFieldConv)
					return
				}
			} else if !srcPtr && tgtPtr {
				deref = "&"
			}
			srcFieldConv, ok := t.ConvertType(srcField, srcc.Attribute().Type)
			if ok {
				// type conversion required. Add it in postinit code.
				tgtName := tgtc.Identifier(tgtMatt.ElemName(n), false)
				postInitCode += fmt.Sprintf("%sptr := %s\n%s = %s%sptr\n", tgtName, srcFieldConv, t.TargetVar(tgtField), deref, tgtName)
				return
			}
			initCode += fmt.Sprintf("\n%s: %s%s,", tgtField, deref, srcFieldConv)
		})
		if initCode != "" {
			initCode += "\n"
		}
	}

	buffer := &bytes.Buffer{}
	deref := "&"
	// if the target is a raw struct no need to return a pointer
	if _, ok := target.Attribute().Type.(*expr.Object); ok {
		deref = ""
	}
	buffer.WriteString(fmt.Sprintf("%s %s %s%s{%s}\n", t.TargetVar(""), t.Assign(), deref,
		target.Name(true), initCode))
	buffer.WriteString(postInitCode)

	// iterate through non-primitive attributes to initialize rest of the
	// struct fields
	var err error
	walkMatches(source, target, func(srcMatt, tgtMatt *expr.MappedAttributeExpr, srcc, tgtc AttributeAnalyzer, n string) {
		srccAtt := srcc.Attribute()
		tgtcAtt := tgtc.Attribute()
		srcVar := t.SourceVar(srcc.Identifier(srcMatt.ElemName(n), true))
		tgtVar := t.TargetVar(tgtc.Identifier(tgtMatt.ElemName(n), true))
		if err = isCompatible(srccAtt.Type, tgtcAtt.Type, srcVar, tgtVar); err != nil {
			return
		}

		var (
			code string

			newT = t.Dup(srcVar, tgtVar, false)
		)
		{
			_, ok := srccAtt.Type.(expr.UserType)
			switch {
			case expr.IsArray(srccAtt.Type):
				code, err = transformArray(expr.AsArray(srccAtt.Type), expr.AsArray(tgtcAtt.Type), srcc, tgtc, newT)
			case expr.IsMap(srccAtt.Type):
				code, err = transformMap(expr.AsMap(srccAtt.Type), expr.AsMap(tgtcAtt.Type), srcc, tgtc, newT)
			case ok:
				code = fmt.Sprintf("%s = %s(%s)\n", tgtVar, newT.HelperName(srcc, tgtc), srcVar)
			case expr.IsObject(srccAtt.Type):
				code, err = transformAttribute(srcc, tgtc, newT)
			}
		}
		if err != nil {
			return
		}

		// We need to check for a nil source if it holds a reference (pointer to
		// primitive or an object, array or map) and is not required. We also want
		// to always check nil if the attribute is not a primitive; it's a
		// 1) user type and we want to avoid calling transform helper functions
		// with nil value
		// 2) it's an object, map or array to avoid making empty arrays and maps
		// and to avoid derefencing nil.
		var checkNil bool
		{
			checkNil = srcc.IsPointer()
			if !checkNil && !expr.IsPrimitive(srccAtt.Type) {
				if !srcc.IsRequired() && srcc.DefaultValue() == nil {
					checkNil = true
				}
			}
		}
		if code != "" && checkNil {
			code = fmt.Sprintf("if %s != nil {\n\t%s}\n", srcVar, code)
		}

		// Default value handling. We need to handle default values if the target
		// type uses default values (i.e. attributes with default values are
		// non-pointers) and has a default value set.
		if tdef := tgtc.DefaultValue(); tdef != nil {
			if srcc.IsPointer() {
				code += fmt.Sprintf("if %s == nil {\n\t", srcVar)
				if tgtc.IsPointer() {
					code += fmt.Sprintf("var tmp %s = %#v\n\t%s = &tmp\n", tgtc.Def(), tdef, tgtVar)
				} else {
					code += fmt.Sprintf("%s = %#v\n", tgtVar, tdef)
				}
				code += "}\n"
			}
		}

		buffer.WriteString(code)
	})
	if err != nil {
		return "", err
	}

	return buffer.String(), nil
}

func transformArray(source, target *expr.Array, sourceAn, targetAn AttributeAnalyzer, t Transformer) (string, error) {
	var err error
	sElemAn := sourceAn.Dup(source.ElemType)
	tElemAn := targetAn.Dup(target.ElemType)
	elemRef := tElemAn.Ref(true)
	if sElemAn, tElemAn, err = t.MakeCompatible(sElemAn, tElemAn, "[0]"); err != nil {
		return "", err
	}
	data := map[string]interface{}{
		"Transformer": t,
		"ElemTypeRef": elemRef,
		"SourceElem":  sElemAn,
		"TargetElem":  tElemAn,
		"LoopVar":     string(105 + strings.Count(t.TargetVar(""), "[")),
	}
	var buf bytes.Buffer
	if err := transformArrayT.Execute(&buf, data); err != nil {
		panic(err) // bug
	}
	return buf.String(), nil
}

func transformMap(source, target *expr.Map, sourceAn, targetAn AttributeAnalyzer, t Transformer) (string, error) {
	var err error
	sKeyAn := sourceAn.Dup(source.KeyType)
	tKeyAn := targetAn.Dup(target.KeyType)
	keyRef := tKeyAn.Ref(true)
	if sKeyAn, tKeyAn, err = t.MakeCompatible(sKeyAn, tKeyAn, "[key]"); err != nil {
		return "", err
	}
	sElemAn := sourceAn.Dup(source.ElemType)
	tElemAn := targetAn.Dup(target.ElemType)
	elemRef := tElemAn.Ref(true)
	if sElemAn, tElemAn, err = t.MakeCompatible(sElemAn, tElemAn, "[*]"); err != nil {
		return "", err
	}
	data := map[string]interface{}{
		"Transformer": t,
		"KeyTypeRef":  keyRef,
		"ElemTypeRef": elemRef,
		"SourceKey":   sKeyAn,
		"TargetKey":   tKeyAn,
		"SourceElem":  sElemAn,
		"TargetElem":  tElemAn,
		"LoopVar":     "",
	}
	if depth := mapDepth(target); depth > 0 {
		data["LoopVar"] = string(97 + depth)
	}
	var buf bytes.Buffer
	if err := transformMapT.Execute(&buf, data); err != nil {
		panic(err) // bug
	}
	return buf.String(), nil
}

// mapDepth returns the level of nested maps. If map not nested, it returns 0.
func mapDepth(mp *expr.Map) int {
	return traverseMap(mp.ElemType.Type, 0)
}

func traverseMap(dt expr.DataType, depth int, seen ...map[string]struct{}) int {
	if mp := expr.AsMap(dt); mp != nil {
		depth++
		depth = traverseMap(mp.ElemType.Type, depth, seen...)
	} else if ar := expr.AsArray(dt); ar != nil {
		depth = traverseMap(ar.ElemType.Type, depth, seen...)
	} else if mo := expr.AsObject(dt); mo != nil {
		var s map[string]struct{}
		if len(seen) > 0 {
			s = seen[0]
		} else {
			s = make(map[string]struct{})
			seen = append(seen, s)
		}
		key := dt.Name()
		if u, ok := dt.(expr.UserType); ok {
			key = u.ID()
		}
		if _, ok := s[key]; ok {
			return depth
		}
		s[key] = struct{}{}
		var level int
		for _, nat := range *mo {
			// if object type has attributes of type map then find out the attribute that has
			// the deepest level of nested maps
			lvl := 0
			lvl = traverseMap(nat.Attribute.Type, lvl, seen...)
			if lvl > level {
				level = lvl
			}
		}
		depth += level
	}
	return depth
}

// transformAttributeHelpers returns the transform functions required to
// transform source data type to target. It returns an error if source and
// target are not compatible.
func transformAttributeHelpers(source, target AttributeAnalyzer, t Transformer, seen ...map[string]*TransformFunctionData) ([]*TransformFunctionData, error) {
	var err error
	if source, target, err = t.MakeCompatible(source, target, ""); err != nil {
		return nil, err
	}

	var (
		helpers []*TransformFunctionData

		sourceType = source.Attribute().Type
		targetType = target.Attribute().Type
	)
	{
		// Do not generate a transform function for the top most user type.
		switch {
		case expr.IsArray(sourceType):
			source = source.Dup(expr.AsArray(sourceType).ElemType)
			target = target.Dup(expr.AsArray(targetType).ElemType)
			helpers, err = transformAttributeHelpers(source, target, t, seen...)
		case expr.IsMap(sourceType):
			sm := expr.AsMap(sourceType)
			tm := expr.AsMap(targetType)
			source = source.Dup(sm.ElemType)
			target = target.Dup(tm.ElemType)
			helpers, err = transformAttributeHelpers(source, target, t, seen...)
			if err == nil {
				var other []*TransformFunctionData
				source = source.Dup(sm.KeyType)
				target = target.Dup(tm.KeyType)
				other, err = transformAttributeHelpers(source, target, t, seen...)
				helpers = AppendHelpers(helpers, other)
			}
		case expr.IsObject(sourceType):
			helpers, err = transformObjectHelpers(source, target, t, seen...)
		}
	}
	if err != nil {
		return nil, err
	}
	return helpers, nil
}

// transformObjectHelpers collects the helper functions required to transform
// source object type to target object.
func transformObjectHelpers(source, target AttributeAnalyzer, t Transformer, seen ...map[string]*TransformFunctionData) ([]*TransformFunctionData, error) {
	var (
		helpers []*TransformFunctionData
		err     error
	)
	walkMatches(source, target, func(srcMatt, tgtMatt *expr.MappedAttributeExpr, srcc, tgtc AttributeAnalyzer, n string) {
		if err != nil {
			return
		}
		h, err2 := collectHelpers(srcc, tgtc, t, seen...)
		if err2 != nil {
			err = err2
			return
		}
		helpers = append(helpers, h...)
	})
	if err != nil {
		return nil, err
	}
	return helpers, nil
}

// collectHelpers recursively traverses the given attributes and return the
// transform helper functions required to generate the transform code.
func collectHelpers(source, target AttributeAnalyzer, t Transformer, seen ...map[string]*TransformFunctionData) ([]*TransformFunctionData, error) {
	var (
		data []*TransformFunctionData

		sourceType = source.Attribute().Type
		targetType = target.Attribute().Type
	)
	switch {
	case expr.IsArray(sourceType):
		source = source.Dup(expr.AsArray(sourceType).ElemType)
		target = target.Dup(expr.AsArray(targetType).ElemType)
		helpers, err := transformAttributeHelpers(source, target, t, seen...)
		if err != nil {
			return nil, err
		}
		data = AppendHelpers(data, helpers)
	case expr.IsMap(sourceType):
		source = source.Dup(expr.AsMap(sourceType).KeyType)
		target = target.Dup(expr.AsMap(targetType).KeyType)
		helpers, err := transformAttributeHelpers(source, target, t, seen...)
		if err != nil {
			return nil, err
		}
		data = AppendHelpers(data, helpers)
		source = source.Dup(expr.AsMap(sourceType).ElemType)
		target = target.Dup(expr.AsMap(targetType).ElemType)
		helpers, err = transformAttributeHelpers(source, target, t, seen...)
		if err != nil {
			return nil, err
		}
		data = AppendHelpers(data, helpers)
	case expr.IsObject(sourceType):
		if ut, ok := sourceType.(expr.UserType); ok {
			name := t.HelperName(source, target)
			var s map[string]*TransformFunctionData
			if len(seen) > 0 {
				s = seen[0]
			} else {
				s = make(map[string]*TransformFunctionData)
				seen = append(seen, s)
			}
			if _, ok := s[name]; ok {
				return nil, nil
			}
			newT := t.Dup("v", "res", true)
			code, err := transformAttribute(source.Dup(ut.Attribute()), target, newT)
			if err != nil {
				return nil, err
			}
			if !source.IsRequired() {
				code = "if v == nil {\n\treturn nil\n}\n" + code
			}
			tfd := &TransformFunctionData{
				Name:          t.HelperName(source, target),
				ParamTypeRef:  source.Ref(true),
				ResultTypeRef: target.Ref(true),
				Code:          code,
			}
			s[name] = tfd
			data = AppendHelpers(data, []*TransformFunctionData{tfd})
		}

		// collect helpers
		var err error
		{
			walkMatches(source, target, func(srcMatt, _ *expr.MappedAttributeExpr, srcc, tgtc AttributeAnalyzer, n string) {
				var helpers []*TransformFunctionData
				helpers, err = collectHelpers(srcc, tgtc, t, seen...)
				if err != nil {
					return
				}
				data = AppendHelpers(data, helpers)
			})
		}
		if err != nil {
			return nil, err
		}
	}
	return data, nil
}

// isCompatible returns an error if a and b are not both objects, both arrays,
// both maps or both the same primitive type. actx and bctx are used to build
// the error message if any.
func isCompatible(a, b expr.DataType, actx, bctx string) error {
	switch {
	case expr.IsObject(a):
		if !expr.IsObject(b) {
			return fmt.Errorf("%s is an object but %s type is %s", actx, bctx, b.Name())
		}
	case expr.IsArray(a):
		if !expr.IsArray(b) {
			return fmt.Errorf("%s is an array but %s type is %s", actx, bctx, b.Name())
		}
	case expr.IsMap(a):
		if !expr.IsMap(b) {
			return fmt.Errorf("%s is a hash but %s type is %s", actx, bctx, b.Name())
		}
	default:
		if a.Kind() != b.Kind() {
			return fmt.Errorf("%s is a %s but %s type is %s", actx, a.Name(), bctx, b.Name())
		}
	}
	return nil
}

// walkMatches iterates through the source attribute expression and executes
// the walker function.
func walkMatches(source, target AttributeAnalyzer, walker func(src, tgt *expr.MappedAttributeExpr, srcc, tgtc AttributeAnalyzer, n string)) {
	srcMatt := expr.NewMappedAttributeExpr(source.Attribute())
	tgtMatt := expr.NewMappedAttributeExpr(target.Attribute())
	sProp := source.Properties()
	tProp := target.Properties()
	srcObj := expr.AsObject(srcMatt.Type)
	tgtObj := expr.AsObject(tgtMatt.Type)
	for _, nat := range *srcObj {
		if att := tgtObj.Attribute(nat.Name); att != nil {
			srcc := source.Dup(nat.Attribute)
			srcc.SetProperties(srcMatt.IsRequired(nat.Name), sProp.Pointer, sProp.UseDefault)
			tgtc := target.Dup(att)
			tgtc.SetProperties(tgtMatt.IsRequired(nat.Name), tProp.Pointer, tProp.UseDefault)
			walker(srcMatt, tgtMatt, srcc, tgtc, nat.Name)
		}
	}
}

// used by template
func transformAttributeHelper(source, target AttributeAnalyzer, sourceVar, targetVar string, newVar bool, t Transformer) (string, error) {
	newT := t.Dup(sourceVar, targetVar, newVar)
	return transformAttribute(source, target, newT)
}

const transformArrayTmpl = `{{ .Transformer.TargetVar "" }} {{ .Transformer.Assign }} make([]{{ .ElemTypeRef }}, len({{ .Transformer.SourceVar "" }}))
for {{ .LoopVar }}, val := range {{ .Transformer.SourceVar "" }} {
	{{ transformAttribute .SourceElem .TargetElem "val" (printf "%s[%s]" (.Transformer.TargetVar "") .LoopVar) false .Transformer -}}
}
`

const transformMapTmpl = `{{ .Transformer.TargetVar "" }} {{ .Transformer.Assign }} make(map[{{ .KeyTypeRef }}]{{ .ElemTypeRef }}, len({{ .Transformer.SourceVar "" }}))
for key, val := range {{ .Transformer.SourceVar "" }} {
	{{ transformAttribute .SourceKey .TargetKey "key" "tk" true .Transformer -}}
	{{ transformAttribute .SourceElem .TargetElem "val" (printf "tv%s" .LoopVar) true .Transformer -}}
	{{ .Transformer.TargetVar "" }}[tk] = {{ printf "tv%s" .LoopVar }}
}
`
