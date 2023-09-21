use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self, CodeLocation, GetLocation, GetName, Ident};
use semantic_analyzer::types::semantic::SemanticStackContext;
use semantic_analyzer::types::types::{
    PrimitiveTypes, StructType, StructTypes, Type, TypeAttributes,
};

mod utils;

#[test]
fn types_ast_transform() {
    let type_ast = ast::StructTypes {
        name: Ident::new("type1"),
        attributes: vec![],
    };
    assert_eq!(type_ast.name(), "type1");
    assert_eq!(type_ast.location(), CodeLocation::new(1, 0));
    let main_ast_type = ast::Type::Struct(type_ast.clone());
    assert_eq!(main_ast_type.name(), "type1");

    let type_into1: StructTypes = type_ast.clone().into();
    assert_eq!(type_into1.name, "type1");
    assert!(type_into1.attributes.is_empty());
    assert!(type_into1.methods.is_empty());

    let ty1 = ast::StructType {
        attr_name: Ident::new("attr1"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::U8),
    };
    let ty2 = ast::StructType {
        attr_name: Ident::new("attr2"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::U16),
    };
    let ty3 = ast::StructType {
        attr_name: Ident::new("attr3"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::U32),
    };
    let ty4 = ast::StructType {
        attr_name: Ident::new("attr4"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::U64),
    };
    let ty5 = ast::StructType {
        attr_name: Ident::new("attr5"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::I8),
    };
    let ty6 = ast::StructType {
        attr_name: Ident::new("attr6"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::I16),
    };
    let ty7 = ast::StructType {
        attr_name: Ident::new("attr7"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::I32),
    };
    let ty8 = ast::StructType {
        attr_name: Ident::new("attr8"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::I64),
    };
    let ty9 = ast::StructType {
        attr_name: Ident::new("attr9"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let ty10 = ast::StructType {
        attr_name: Ident::new("attr10"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::F32),
    };
    let ty11 = ast::StructType {
        attr_name: Ident::new("attr11"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::F64),
    };
    let ty12 = ast::StructType {
        attr_name: Ident::new("attr12"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Char),
    };
    let ty13 = ast::StructType {
        attr_name: Ident::new("attr13"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::String),
    };
    let ty14 = ast::StructType {
        attr_name: Ident::new("attr14"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Ptr),
    };
    let ty15 = ast::StructType {
        attr_name: Ident::new("attr15"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::None),
    };
    let type_ast = ast::StructTypes {
        name: Ident::new("type2"),
        attributes: vec![
            ty1.clone(),
            ty2.clone(),
            ty3.clone(),
            ty4.clone(),
            ty5.clone(),
            ty6.clone(),
            ty7.clone(),
            ty8.clone(),
            ty9.clone(),
            ty10.clone(),
            ty11.clone(),
            ty12.clone(),
            ty13.clone(),
            ty14.clone(),
            ty15.clone(),
        ],
    };
    let type_into2: StructTypes = type_ast.clone().into();
    assert_eq!(type_into2.name, "type2");
    assert_eq!(type_into2.attributes.len(), 15);

    // Index=0 the same, so we can check directly
    assert_eq!(ty1.attr_type.name(), "u8");
    let attr1 = type_into2.attributes.get(&("attr1".into())).unwrap();
    let ty1: StructType = ty1.into();
    assert_eq!(attr1, &ty1);
    assert_eq!(type_into2.get_attribute_index(&("attr2".into())), Some(1));
    assert_eq!(
        type_into2.get_attribute_type(&("attr2".into())),
        Some(Type::Primitive(PrimitiveTypes::U16))
    );
    assert_eq!(attr1.attr_name, ty1.attr_name);
    assert_eq!(attr1.attr_type, ty1.attr_type);
    assert_eq!(attr1.attr_type.name().to_string(), "u8");
    assert_eq!(attr1.attr_type.name().name(), "u8");
    assert_eq!(attr1.attr_index, 0);

    let attr2 = type_into2.attributes.get(&("attr2".into())).unwrap();
    assert_eq!(ty2.attr_type.name(), "u16");
    let ty2: StructType = ty2.into();
    assert_eq!(attr2.attr_name, ty2.attr_name);
    assert_eq!(attr2.attr_type, ty2.attr_type);
    assert_eq!(attr2.attr_type.to_string(), "u16");
    assert_eq!(attr2.attr_index, 1);

    let attr3 = type_into2.attributes.get(&("attr3".into())).unwrap();
    assert_eq!(ty3.attr_type.name(), "u32");
    let ty3: StructType = ty3.into();
    assert_eq!(attr3.attr_name, ty3.attr_name);
    assert_eq!(attr3.attr_type, ty3.attr_type);
    assert_eq!(attr3.attr_type.to_string(), "u32");
    assert_eq!(attr3.attr_index, 2);

    let attr4 = type_into2.attributes.get(&("attr4".into())).unwrap();
    assert_eq!(ty4.attr_type.name(), "u64");
    let ty4: StructType = ty4.into();
    assert_eq!(attr4.attr_name, ty4.attr_name);
    assert_eq!(attr4.attr_type, ty4.attr_type);
    assert_eq!(attr4.attr_type.to_string(), "u64");
    assert_eq!(attr4.attr_index, 3);

    let attr5 = type_into2.attributes.get(&("attr5".into())).unwrap();
    assert_eq!(ty5.attr_type.name(), "i8");
    let ty5: StructType = ty5.into();
    assert_eq!(attr5.attr_name, ty5.attr_name);
    assert_eq!(attr5.attr_type, ty5.attr_type);
    assert_eq!(attr5.attr_type.to_string(), "i8");
    assert_eq!(attr5.attr_index, 4);

    let attr6 = type_into2.attributes.get(&("attr6".into())).unwrap();
    assert_eq!(ty6.attr_type.name(), "i16");
    let ty6: StructType = ty6.into();
    assert_eq!(attr6.attr_name, ty6.attr_name);
    assert_eq!(attr6.attr_type, ty6.attr_type);
    assert_eq!(attr6.attr_type.to_string(), "i16");
    assert_eq!(attr6.attr_index, 5);

    let attr7 = type_into2.attributes.get(&("attr7".into())).unwrap();
    assert_eq!(ty7.attr_type.name(), "i32");
    let ty7: StructType = ty7.into();
    assert_eq!(attr7.attr_name, ty7.attr_name);
    assert_eq!(attr7.attr_type, ty7.attr_type);
    assert_eq!(attr7.attr_type.to_string(), "i32");
    assert_eq!(attr7.attr_index, 6);

    let attr8 = type_into2.attributes.get(&("attr8".into())).unwrap();
    assert_eq!(ty8.attr_type.name(), "i64");
    let ty8: StructType = ty8.into();
    assert_eq!(attr8.attr_name, ty8.attr_name);
    assert_eq!(attr8.attr_type, ty8.attr_type);
    assert_eq!(attr8.attr_type.to_string(), "i64");
    assert_eq!(attr8.attr_index, 7);

    let attr9 = type_into2.attributes.get(&("attr9".into())).unwrap();
    assert_eq!(ty9.attr_type.name(), "bool");
    let ty9: StructType = ty9.into();
    assert_eq!(attr9.attr_name, ty9.attr_name);
    assert_eq!(attr9.attr_type, ty9.attr_type);
    assert_eq!(attr9.attr_type.to_string(), "bool");
    assert_eq!(attr9.attr_index, 8);

    //=======================
    // Common type tests
    let pty = Type::Primitive(PrimitiveTypes::U16);
    assert_eq!(pty.get_struct(), None);
    assert_eq!(pty.get_attribute_type(&("attr1".into())), None);
    assert_eq!(pty.get_attribute_index(&("attr1".into())), None);
    assert_eq!(pty.get_method("fn1".to_string()), None);
    assert!(!pty.is_attribute(&("attr1".into())));
    assert!(!pty.is_method("fn1".to_string()));

    let main_type: Type = Type::Struct(type_into2.clone());
    assert_eq!(&type_into2, &main_type.get_struct().unwrap());
    assert_eq!(
        main_type.get_attribute_type(&("attr3".into())).unwrap(),
        Type::Primitive(PrimitiveTypes::U32)
    );
    assert_eq!(main_type.get_attribute_index(&("attr3".into())).unwrap(), 2);
    assert_eq!(main_type.get_method("fn1".to_string()), None);
    assert!(main_type.is_attribute(&("attr3".into())));
    assert!(!main_type.is_method("fn1".to_string()));
}

#[test]
fn types_declaration() {
    let mut t = SemanticTest::new();
    let type_decl = ast::StructTypes {
        name: Ident::new("type1"),
        attributes: vec![],
    };
    t.state.types(&type_decl.clone());
    assert!(t.is_empty_error());

    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::Types {
            type_decl: type_decl.into()
        }
    );
}

#[test]
fn code_location() {
    let x = ast::CodeLocation::new(10, 100);
    assert_eq!(x.line(), 10);
    assert_eq!(x.offset(), 100);
}
