package codegen

import (
	"goa.design/goa/expr"
)

type (
	// AttributeAnalyzer analyzes an attribute and its sub-attributes.
	AttributeAnalyzer interface {
		// IsPointer returns true if the attribute being analyzed is a pointer.
		IsPointer() bool
		// IsRequired returns true if the Required property is set to true.
		IsRequired() bool
		// DefaultValue returns the default value for the attribute being analyzed.
		DefaultValue() interface{}
		// Name returns the type name for the attribute. If withPkg is true, it
		// prefixes the type name with the package name.
		Name(withPkg bool) string
		// Ref returns the type reference for the attribute. If withPkg is true, it
		// prefixes the type ref with the package name.
		Ref(withPkg bool) string
		// Def returns the type definition for the attribute.
		Def() string
		// Identifier returns a valid identifier for the name and attribute
		// expression.
		Identifier(name string, firstUpper bool) string
		// Attribute returns the attribute being analyzed.
		Attribute() *expr.AttributeExpr
		// Properties returns the attribute properties used in analysis.
		Properties() *AttributeProperties
		// Dup creates a shallow copy of the analyzer with the given attribute.
		Dup(att *expr.AttributeExpr) AttributeAnalyzer
		// SetAttribute sets the attribute to analyze.
		SetAttribute(att *expr.AttributeExpr)
		// SetProperties sets the attribute properties in the analyzer.
		SetProperties(required, pointer, useDefault bool)
	}

	// AttributeProperties contains properties that affect how the attribute
	// is stored (i.e. pointer or non-pointer).
	AttributeProperties struct {
		// Pointer if true indicates that the attribute is a pointer even if
		// required or has a default value (except array and map types which are
		// always non-pointers)
		Pointer bool
		// UseDefault if true indicates that the attribute is a non-pointer if it
		// has a default value.
		UseDefault bool
		// Required if true indicates that the attribute is required.
		Required bool
	}

	// Analyzer implements the AttributeAnalyzer interface.
	Analyzer struct {
		// AttributeExpr is the attribute being analyzed.
		AttributeExpr *expr.AttributeExpr
		// AttributeProperties is the set of attribute properties.
		AttributeProperties *AttributeProperties
		// PkgName is the name of the package where the generated type exists.
		PkgName string
		// Scope is the named scope.
		Scope *NameScope
	}
)

// NewAttributeAnalyzer returns a new attribute analyzer.
func NewAttributeAnalyzer(att *expr.AttributeExpr, required, pointer, useDefault bool, pkg string, scope *NameScope) AttributeAnalyzer {
	return &Analyzer{
		AttributeExpr: att,
		AttributeProperties: &AttributeProperties{
			Required:   required,
			Pointer:    pointer,
			UseDefault: useDefault,
		},
		PkgName: pkg,
		Scope:   scope,
	}
}

// IsPointer checks if the attribute is a pointer.
//
// The following table shows how the attribute properties affect the return
// value
//
//    Pointer | UseDefault | Required | IsPointer
//       T    |      F     |     F    |     T
//       T    |      T     |     F    |     T
//       T    |      F     |     T    |     T
//       F    |      T     |     F    |     F if default value exists / T otherwise
//       F    |      T     |     T    |     F
//       F    |      F     |     T    |     F
//
func (a *Analyzer) IsPointer() bool {
	if a.AttributeProperties.Pointer {
		return true
	}
	if kind := a.AttributeExpr.Type.Kind(); kind == expr.BytesKind || kind == expr.AnyKind {
		return false
	}
	return !a.IsRequired() &&
		(a.AttributeExpr.DefaultValue == nil || !a.AttributeProperties.UseDefault)
}

// IsRequired returns the Required attribute property.
func (a *Analyzer) IsRequired() bool {
	return a.AttributeProperties.Required
}

// DefaultValue returns the default value of the attribute being analyzed if
// UseDefault is set to true. It returns nil otherwise.
func (a *Analyzer) DefaultValue() interface{} {
	if a.AttributeProperties.UseDefault {
		return a.AttributeExpr.DefaultValue
	}
	return nil
}

// Name returns the type name for the given attribute.
func (a *Analyzer) Name(withPkg bool) string {
	if withPkg {
		return a.Scope.GoFullTypeName(a.AttributeExpr, a.PkgName)
	}
	return a.Scope.GoTypeName(a.AttributeExpr)
}

// Ref returns the type reference for the given attribute.
func (a *Analyzer) Ref(withPkg bool) string {
	if withPkg {
		return a.Scope.GoFullTypeRef(a.AttributeExpr, a.PkgName)
	}
	return a.Scope.GoTypeRef(a.AttributeExpr)
}

// Def returns the type definition for the given attribute.
func (a *Analyzer) Def() string {
	return a.Scope.GoTypeDef(a.AttributeExpr, a.AttributeProperties.Pointer, a.AttributeProperties.UseDefault)
}

// Identifier returns a valid identifier for the name and attribute
// expression.
func (a *Analyzer) Identifier(name string, firstUpper bool) string {
	return GoifyAtt(a.AttributeExpr, name, firstUpper)
}

// Attribute returns the inner attribute expression.
func (a *Analyzer) Attribute() *expr.AttributeExpr {
	return a.AttributeExpr
}

// Properties returns the attribute properties used in analysis.
func (a *Analyzer) Properties() *AttributeProperties {
	return a.AttributeProperties
}

// Dup creates a shallow copy of the analyzer with the given attribute.
func (a *Analyzer) Dup(att *expr.AttributeExpr) AttributeAnalyzer {
	return &Analyzer{
		AttributeExpr:       att,
		AttributeProperties: a.AttributeProperties,
		PkgName:             a.PkgName,
		Scope:               a.Scope,
	}
}

// SetAttribute sets the attribute to analyze.
func (a *Analyzer) SetAttribute(att *expr.AttributeExpr) {
	a.AttributeExpr = att
}

// SetProperties sets the attribute properties in the analyzer.
func (a *Analyzer) SetProperties(required, pointer, useDefault bool) {
	a.AttributeProperties = &AttributeProperties{
		Required:   required,
		Pointer:    pointer,
		UseDefault: useDefault,
	}
}
