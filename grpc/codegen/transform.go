package codegen

import (
	//"bytes"
	"fmt"
	//"strings"

	"goa.design/goa/codegen"
	"goa.design/goa/expr"
)

// protoTransformer implements the codegen.Transformer interface.
type protoTransformAttrs struct {
	*codegen.TransformAttrs
	// proto if true indicates target type is a protocol buffer type.
	proto bool
}

// protoBufTransform transforms goa type to protocol buffer type and
// vice versa.
//
// `proto` param if true indicates that the source type is transformed
// into a protocol buffer type.
func protoBufTransform(source, target codegen.AttributeAnalyzer, sourceVar, targetVar string, proto bool) (string, []*codegen.TransformFunctionData, error) {
	var prefix string
	{
		prefix = "protobuf"
		if proto {
			prefix = "svc"
		}
	}
	p := &protoTransformAttrs{
		TransformAttrs: &codegen.TransformAttrs{
			SourceVarn:   sourceVar,
			TargetVarn:   targetVar,
			HelperPrefix: prefix,
			NewVar:       true,
		},
		proto: proto,
	}
	return codegen.TransformWith(source, target, p)
}

func (p *protoTransformAttrs) MakeCompatible(source, target codegen.AttributeAnalyzer, suffix string) (src, tgt codegen.AttributeAnalyzer, err error) {
	if source, target, err = p.TransformAttrs.MakeCompatible(source, target, suffix); err != nil {
		if p.proto {
			tAtt := unwrapAttr(&expr.AttributeExpr{Type: target.Attribute().Type})
			target.SetAttribute(tAtt)
			p.TargetVarn += ".Field"
			p.NewVar = false
		} else {
			sAtt := unwrapAttr(&expr.AttributeExpr{Type: source.Attribute().Type})
			source.SetAttribute(sAtt)
			p.SourceVarn += ".Field"
		}
		if source, target, err = p.TransformAttrs.MakeCompatible(source, target, ""); err != nil {
			return source, target, err
		}
	}
	return source, target, nil
}

// ConvertType converts varn to type typ.
// NOTE: For Int and UInt kinds, protocol buffer Go compiler generates
// int32 and uint32 respectively whereas goa v2 generates int and uint.
func (p *protoTransformAttrs) ConvertType(varn string, typ expr.DataType) (string, bool) {
	if typ.Kind() != expr.IntKind && typ.Kind() != expr.UIntKind {
		return varn, false
	}

	if p.proto {
		return fmt.Sprintf("%s(%s)", protoBufNativeGoTypeName(typ), varn), true
	}
	return fmt.Sprintf("%s(%s)", codegen.GoNativeTypeName(typ), varn), true
}

func (p *protoTransformAttrs) Dup(sourceVar, targetVar string, newVar bool) codegen.Transformer {
	return &protoTransformAttrs{
		TransformAttrs: &codegen.TransformAttrs{
			SourceVarn:   sourceVar,
			TargetVarn:   targetVar,
			HelperPrefix: p.HelperPrefix,
			NewVar:       newVar,
		},
		proto: p.proto,
	}
}

/*// TransformAttribute converts source attribute expression to target returning
// the conversion code and error (if any). Either source or target is a
// protocol buffer message type.
func (p *protoTransformer) TransformAttribute(source, target *expr.AttributeExpr, sourceVar, targetVar, sourcePkg, targetPkg string, newVar bool) (string, error) {
	var (
		code string
		err  error
	)
	{
		svcAtt := target
		if p.proto {
			svcAtt = source
		}
		switch {
		case expr.IsArray(svcAtt.Type):
			code, err = p.transformArray(source, target, sourceVar, targetVar, sourcePkg, targetPkg, newVar)
		case expr.IsMap(svcAtt.Type):
			code, err = p.transformMap(source, target, sourceVar, targetVar, sourcePkg, targetPkg, newVar)
		case expr.IsObject(svcAtt.Type):
			code, err = p.transformObject(source, target, sourceVar, targetVar, sourcePkg, targetPkg, newVar)
		default:
			code, err = p.transformPrimitive(source, target, sourceVar, targetVar, sourcePkg, targetPkg, newVar)
		}
	}
	if err != nil {
		return "", err
	}
	return code, nil
}

func (p *protoTransformer) transformPrimitive(source, target *expr.AttributeExpr, sourceVar, targetVar, sourcePkg, targetPkg string, newVar bool) (string, error) {
	var code string
	if err := codegen.IsCompatible(source.Type, target.Type, sourceVar, targetVar); err != nil {
		if p.proto {
			code += fmt.Sprintf("%s := &%s{}\n", targetVar, protoBufGoFullTypeName(target, targetPkg, p.scope))
			targetVar += ".Field"
			newVar = false
			target = unwrapAttr(target)
		} else {
			source = unwrapAttr(source)
			sourceVar += ".Field"
		}
		if err = codegen.IsCompatible(source.Type, target.Type, sourceVar, targetVar); err != nil {
			return "", err
		}
	}
	assign := "="
	if newVar {
		assign = ":="
	}
	code += fmt.Sprintf("%s %s %s\n", targetVar, assign, typeConvert(sourceVar, source.Type, target.Type, p.proto))
	return code, nil
}

func (p *protoTransformer) transformObject(source, target *expr.AttributeExpr, sourceVar, targetVar, sourcePkg, targetPkg string, newVar bool) (string, error) {
	if err := codegen.IsCompatible(source.Type, target.Type, sourceVar, targetVar); err != nil {
		if p.proto {
			target = unwrapAttr(target)
		} else {
			source = unwrapAttr(source)
		}
		if err = codegen.IsCompatible(source.Type, target.Type, sourceVar, targetVar); err != nil {
			return "", err
		}
	}
	var (
		initCode     string
		postInitCode string

		buffer = &bytes.Buffer{}
	)
	{
		// iterate through attributes of primitive type first to initialize the
		// struct
		codegen.WalkMatches(source, target, func(src, tgt *expr.MappedAttributeExpr, srcAtt, tgtAtt *expr.AttributeExpr, n string) {
			if !expr.IsPrimitive(srcAtt.Type) {
				return
			}
			var (
				srcFldName, tgtFldName string
				srcPtr, tgtPtr         bool
			)
			{
				if p.proto {
					srcPtr = source.IsPrimitivePointer(n, true)
					srcFldName = codegen.Goify(src.ElemName(n), true)
					tgtFldName = protoBufify(tgt.ElemName(n), true)
				} else {
					srcFldName = protoBufify(src.ElemName(n), true)
					tgtFldName = codegen.Goify(tgt.ElemName(n), true)
					tgtPtr = target.IsPrimitivePointer(n, true)
				}
			}
			deref := ""
			srcField := sourceVar + "." + srcFldName
			switch {
			case srcPtr && !tgtPtr:
				if !source.IsRequired(n) {
					postInitCode += fmt.Sprintf("if %s != nil {\n\t%s.%s = %s\n}\n",
						srcField, targetVar, tgtFldName, typeConvert("*"+srcField, srcAtt.Type, tgtAtt.Type, p.proto))
					return
				}
				deref = "*"
			case !srcPtr && tgtPtr:
				deref = "&"
				if sVar := typeConvert(srcField, srcAtt.Type, tgtAtt.Type, p.proto); sVar != srcField {
					// type cast is required
					tgtName := codegen.Goify(tgt.ElemName(n), false)
					postInitCode += fmt.Sprintf("%sptr := %s\n%s.%s = %s%sptr\n", tgtName, sVar, targetVar, tgtFldName, deref, tgtName)
					return
				}
			}
			initCode += fmt.Sprintf("\n%s: %s%s,", tgtFldName, deref, typeConvert(srcField, srcAtt.Type, tgtAtt.Type, p.proto))
		})
	}
	if initCode != "" {
		initCode += "\n"
	}
	assign := "="
	if newVar {
		assign = ":="
	}
	deref := "&"
	// if the target is a raw struct no need to return a pointer
	if _, ok := target.Type.(*expr.Object); ok {
		deref = ""
	}
	buffer.WriteString(fmt.Sprintf("%s %s %s%s{%s}\n", targetVar, assign, deref,
		p.scope.GoFullTypeName(target, targetPkg), initCode))
	buffer.WriteString(postInitCode)

	var err error
	{
		codegen.WalkMatches(source, target, func(src, tgt *expr.MappedAttributeExpr, srcAtt, tgtAtt *expr.AttributeExpr, n string) {
			var srcFldName, tgtFldName string
			{
				if p.proto {
					srcFldName = codegen.GoifyAtt(srcAtt, src.ElemName(n), true)
					tgtFldName = protoBufifyAtt(tgtAtt, tgt.ElemName(n), true)
				} else {
					srcFldName = protoBufifyAtt(tgtAtt, tgt.ElemName(n), true)
					tgtFldName = codegen.GoifyAtt(srcAtt, src.ElemName(n), true)
				}
			}
			srcVar := sourceVar + "." + srcFldName
			tgtVar := targetVar + "." + tgtFldName
			err = codegen.IsCompatible(srcAtt.Type, tgtAtt.Type, srcVar, tgtVar)
			if err != nil {
				return
			}

			var (
				code string
			)
			{
				_, ok := srcAtt.Type.(expr.UserType)
				switch {
				case expr.IsArray(srcAtt.Type):
					code, err = p.transformArray(srcAtt, tgtAtt, srcVar, tgtVar, sourcePkg, targetPkg, false)
				case expr.IsMap(srcAtt.Type):
					code, err = p.transformMap(srcAtt, tgtAtt, srcVar, tgtVar, sourcePkg, targetPkg, false)
				case ok:
					code = fmt.Sprintf("%s = %s(%s)\n", tgtVar, p.Helper(srcAtt, tgtAtt), srcVar)
				case expr.IsObject(srcAtt.Type):
					code, err = p.TransformAttribute(srcAtt, tgtAtt, srcVar, tgtVar, sourcePkg, targetPkg, false)
				}
				if err != nil {
					return
				}

				// Nil check handling.
				//
				// We need to check for a nil source if it holds a reference
				// (pointer to primitive or an object, array or map) and is not
				// required. If source is a protocol buffer generated Go type,
				// the attributes of primitive type are always non-pointers (even if
				// not required). We don't have to check for nil in that case.
				var checkNil bool
				{
					checkNil = !expr.IsPrimitive(srcAtt.Type) && !src.IsRequired(n) || src.IsPrimitivePointer(n, true) && !p.proto
				}
				if code != "" && checkNil {
					code = fmt.Sprintf("if %s != nil {\n\t%s}\n", srcVar, code)
				}

				// Default value handling.
				// proto3 does not support non-zero default values. It is impossible to
				// find out from the protocol buffer type whether the primitive fields
				// (always non-pointers) are set to zero values as default or by the
				// application itself.
				if tgt.HasDefaultValue(n) {
					if p.proto {
						if src.IsPrimitivePointer(n, true) || !expr.IsPrimitive(srcAtt.Type) {
							code += fmt.Sprintf("if %s == nil {\n\t", srcVar)
							code += fmt.Sprintf("%s = %#v\n", tgtVar, tgtAtt.DefaultValue)
							code += "}\n"
						}
					} else if !expr.IsPrimitive(srcAtt.Type) {
						code += fmt.Sprintf("if %s == nil {\n\t", srcVar)
						code += fmt.Sprintf("%s = %#v\n", tgtVar, tgtAtt.DefaultValue)
						code += "}\n"
					}
				}
			}
			buffer.WriteString(code)
		})
	}
	if err != nil {
		return "", err
	}
	return buffer.String(), nil
}

func (p *protoTransformer) transformArray(source, target *expr.AttributeExpr, sourceVar, targetVar, sourcePkg, targetPkg string, newVar bool) (string, error) {
	var (
		src, tgt *expr.Array
		tgtInit  string
	)
	{
		src = expr.AsArray(source.Type)
		tgt = expr.AsArray(target.Type)
		if err := codegen.IsCompatible(source.Type, target.Type, sourceVar+"[0]", targetVar+"[0]"); err != nil {
			if p.proto {
				tgt = expr.AsArray(unwrapAttr(target).Type)
				assign := "="
				if newVar {
					assign = ":="
				}
				tgtInit = fmt.Sprintf("%s %s &%s{}\n", targetVar, assign, protoBufGoFullTypeName(target, targetPkg, p.scope))
				targetVar += ".Field"
				newVar = false
			} else {
				src = expr.AsArray(unwrapAttr(source).Type)
				sourceVar += ".Field"
			}
			if _, err := p.TransformAttribute(src.ElemType, tgt.ElemType, sourceVar, targetVar, sourcePkg, targetPkg, newVar); err != nil {
				return "", err
			}
		}
	}
	data := map[string]interface{}{
		"Source":      sourceVar,
		"Target":      targetVar,
		"TargetInit":  tgtInit,
		"NewVar":      newVar,
		"ElemTypeRef": elemRef,
		"SourceElem":  src.ElemType,
		"TargetElem":  tgt.ElemType,
		"SourcePkg":   sourcePkg,
		"TargetPkg":   targetPkg,
		"Transformer": p,
		"LoopVar":     string(105 + strings.Count(targetVar, "[")),
	}
	return codegen.TransformArray(data), nil
}

func (p *protoTransformer) transformMap(source, target *expr.AttributeExpr, sourceVar, targetVar, sourcePkg, targetPkg string, newVar bool) (string, error) {
	var (
		src, tgt *expr.Map
		tgtInit  string
	)
	{
		src = expr.AsMap(source.Type)
		tgt = expr.AsMap(target.Type)
		if err := codegen.IsCompatible(source.Type, target.Type, sourceVar+"[*]", targetVar+"[*]"); err != nil {
			if p.proto {
				tgt = expr.AsMap(unwrapAttr(target).Type)
				assign := "="
				if newVar {
					assign = ":="
				}
				tgtInit = fmt.Sprintf("%s %s &%s{}\n", targetVar, assign, protoBufGoFullTypeName(target, targetPkg, p.scope))
				targetVar += ".Field"
				newVar = false
			} else {
				src = expr.AsMap(unwrapAttr(source).Type)
				sourceVar += ".Field"
			}
			if _, err := p.TransformAttribute(src.ElemType, tgt.ElemType, sourceVar, targetVar, sourcePkg, targetPkg, newVar); err != nil {
				return "", err
			}
		}
	}
	if err := codegen.IsCompatible(src.KeyType.Type, tgt.KeyType.Type, sourceVar+".key", targetVar+".key"); err != nil {
		return "", err
	}
	data := map[string]interface{}{
		"Source":      sourceVar,
		"Target":      targetVar,
		"TargetInit":  tgtInit,
		"NewVar":      newVar,
		"KeyTypeRef":  keyRef,
		"ElemTypeRef": elemRef,
		"SourceKey":   src.KeyType,
		"TargetKey":   tgt.KeyType,
		"SourceElem":  src.ElemType,
		"TargetElem":  tgt.ElemType,
		"SourcePkg":   sourcePkg,
		"TargetPkg":   targetPkg,
		"Transformer": p,
		"LoopVar":     "",
	}
	return codegen.TransformMap(data, tgt), nil
}*/
