#![forbid(unsafe_code)]
#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::cargo,
    clippy::clone_on_ref_ptr,
    clippy::dbg_macro,
    clippy::exit,
    clippy::filetype_is_file,
    clippy::float_cmp_const,
    clippy::get_unwrap,
    clippy::if_then_some_else_none,
    clippy::indexing_slicing,
    clippy::let_underscore_must_use,
    clippy::lossy_float_literal,
    clippy::map_err_ignore,
    clippy::mem_forget,
    clippy::missing_inline_in_public_items,
    clippy::multiple_inherent_impl,
    clippy::panic,
    clippy::pattern_type_mismatch,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::rc_buffer,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::shadow_reuse,
    clippy::str_to_string,
    clippy::string_add,
    clippy::string_to_string,
    clippy::unnecessary_self_imports,
    clippy::unneeded_field_pattern,
    clippy::unwrap_in_result,
    clippy::use_debug,
    clippy::verbose_file_reads,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
#![allow(
    clippy::module_name_repetitions,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::cargo_common_metadata
)]

#[cfg(test)]
mod tests;

#[macro_export]
macro_rules! brucket {
    ((module $name:ident $functions:tt $internal_functions:tt $constants:tt)) => {
        brucket!(@module 0 false $name $functions $internal_functions $constants)
    };
    (($node_id:literal:
        module
        $name:ident
        $functions:tt
        $internal_functions:tt
        $constants:tt
    )) => {
        brucket!(@module $node_id false $name $functions $internal_functions $constants)
    };
    (($node_id:literal:
        static
        module
        $name:ident
        $functions:tt
        $internal_functions:tt
        $constants:tt
    )) => {
        brucket!(@module $node_id true $name $functions $internal_functions $constants)
    };
    (@module
        $node_id:literal
        $static:literal
        $name:ident
        ($($function:tt)*)
        ($($internal_function:tt)*)
        ($($constant:tt)*)
    ) => {
        brucket_ast::Module::new(
            brucket_ast::NodeId($node_id),
            $static,
            stringify!($name).to_owned(),
            vec![$(brucket!($function),)*],
            vec![$(brucket!($internal_function),)*],
            vec![$(brucket!($constant),)*],
        )
    };

    ((constant $name:ident $value:tt)) => {
        brucket!(@constant 0 private $name $value)
    };
    (($node_id:literal: $visibility:ident constant $name:ident $value:tt)) => {
        brucket!(@constant $node_id $visibility $name $value)
    };
    (@constant $node_id:literal $visibility:ident $name:ident $value:tt) => {
        brucket_ast::Constant::new(
            brucket_ast::NodeId($node_id),
            brucket!(@visibility $visibility),
            stringify!($name).to_owned(),
            Box::new(brucket!($value)),
        )
    };

    ((internal_function $name:ident $parameters:tt)) => {
        brucket!(@internal_function 0 private eager $name $parameters any)
    };
    (($node_id:literal:
        $visibility:ident
        $strategy:ident
        internal_function
        $name:ident
        $parameters:tt -> $return_type:tt
    )) => {
        brucket!(@internal_function $node_id $visibility $strategy $name $parameters $return_type)
    };
    (@internal_function
        $node_id:literal
        $visibility:ident
        $strategy:ident
        $name:ident
        $parameters:tt
        $return_type:tt
    ) => {
        brucket_ast::function::InternalFunction::new(
            brucket_ast::NodeId($node_id),
            brucket!(@visibility $visibility),
            brucket!(@strategy $strategy),
            stringify!($name).to_owned(),
            brucket!(@parameters $parameters),
            brucket!(@type $return_type),
        )
    };

    ((function $name:ident $parameters:tt $body:tt)) => {
        brucket!(@function 0 private eager $name $parameters any 0 $body)
    };
    (($node_id:literal:
        $visibility:ident
        $strategy:ident
        function
        $name:ident
        $parameters:tt -> $return_type:tt
        $body_node_id:literal:
        $body:tt
    )) => {
        brucket!(
            @function $node_id $visibility $strategy $name $parameters $return_type $body_node_id
            $body
        )
    };
    (@function
        $node_id:literal
        $visibility:ident
        $strategy:ident
        $name:ident
        $parameters:tt
        $return_type:tt
        $body_node_id:literal
        $body:tt
    ) => {
        brucket_ast::function::Function::new(
            brucket_ast::NodeId($node_id),
            brucket!(@visibility $visibility),
            brucket!(@strategy $strategy),
            stringify!($name).to_owned(),
            brucket!(@lambda $body_node_id $parameters $return_type $body),
        )
    };

    ((@node $node_type:ident $node:tt)) => {
        brucket!(@node $node_type $node)
    };
    (@node $node_type:ident $node:tt) => {
        brucket_ast::Node::$node_type(brucket!($node))
    };

    (@visibility public) => { brucket_ast::Visibility::Public };
    (@visibility private) => { brucket_ast::Visibility::Private };

    (@strategy eager) => { brucket_ast::function::ApplicationStrategy::Eager };
    (@strategy lazy) => { brucket_ast::function::ApplicationStrategy::Lazy };

    ((lambda $parameters:tt $body:tt)) => {
        brucket_ast::Node::Lambda(brucket!(@lambda 0 $parameters any $body))
    };
    (($node_id:literal: lambda $parameters:tt -> $return_type:tt $body:tt)) => {
        brucket_ast::Node::Lambda(brucket!(@lambda $node_id $parameters $return_type $body))
    };
    (@lambda $node_id:literal $parameters:tt $return_type:tt $body:tt) => {
        brucket_ast::lambda::Lambda::new(
            brucket_ast::NodeId($node_id),
            brucket!(@parameters $parameters),
            brucket!(@type $return_type),
            Box::new(brucket!($body)),
        )
    };

    (@parameters [$($name:ident)*]) => {
        vec![$(brucket!(@parameter ($name: any)),)*]
    };
    (@parameters [$($parameter:tt)*]) => {
        vec![$(brucket!(@parameter $parameter),)*]
    };
    (@parameter ($name:ident: $type:tt...)) => {
        brucket_ast::lambda::Parameter::new(
            stringify!($name).to_owned(),
            brucket!(@type $type),
            brucket_ast::lambda::Arity::Variadic
        )
    };
    (@parameter ($name:ident: $type:tt)) => {
        brucket_ast::lambda::Parameter::new(
            stringify!($name).to_owned(),
            brucket!(@type $type),
            brucket_ast::lambda::Arity::Unary
        )
    };

    ((let $name:ident $value:tt $then:tt)) => {
        brucket!((let $name: any $value $then))
    };
    ((let $name:ident: $value_type:tt $value:tt $then:tt)) => {
        brucket!((0: let $name: $value_type $value $then))
    };
    (($node_id:literal: let $name:ident: $value_type:tt $value:tt $then:tt)) => {
        brucket_ast::Node::Let(brucket_ast::Let::new(
            brucket_ast::NodeId($node_id),
            stringify!($name).to_owned(),
            brucket!(@type $value_type),
            Box::new(brucket!($value)),
            Box::new(brucket!($then)),
        ))
    };

    (@type any) => { brucket_ast::ast_type::Type::Any };
    (@type unit) => { brucket_ast::ast_type::Type::Unit };
    (@type bool) => { brucket_ast::ast_type::Type::Boolean };
    (@type int) => { brucket_ast::ast_type::Type::Integer };
    (@type float) => { brucket_ast::ast_type::Type::Float };
    (@type str) => { brucket_ast::ast_type::Type::String };
    (@type ($($parameter_type:ident)* -> $return_type:ident)) => {
        brucket!(@type (($($parameter_type)*) -> $return_type))
    };
    (@type (($($parameter_type:tt)*) -> $return_type:ident)) => {
        brucket_ast::ast_type::Type::Lambda(brucket_ast::ast_type::LambdaType::new(
            vec![$(brucket!(@type $parameter_type),)*],
            Box::new(brucket!(@type $return_type))
        ))
    };
    (@type $symbol:ident) => {
        brucket_ast::ast_type::Type::Symbol(stringify!($symbol).to_owned())
    };

    ((if $condition:tt $if_true:tt $if_false:tt)) => {
        brucket!((0: if $condition $if_true $if_false))
    };
    (($node_id:literal: if $condition:tt $if_true:tt $if_false:tt)) => {
        brucket_ast::Node::If(brucket_ast::If::new(
            brucket_ast::NodeId($node_id),
            Box::new(brucket!($condition)),
            Box::new(brucket!($if_true)),
            Box::new(brucket!($if_false)),
        ))
    };

    (()) => {
        brucket_ast::Node::ConstantValue(brucket_ast::constant_value::ConstantValue::new(
            brucket_ast::NodeId(0),
            brucket_ast::constant_value::ConstantVariant::Unit,
        ))
    };

    (null) => {
        brucket_ast::Node::ConstantValue(brucket_ast::constant_value::ConstantValue::new(
            brucket_ast::NodeId(0),
            brucket_ast::constant_value::ConstantVariant::Null,
        ))
    };

    ((($identifier:ident$(::$remaining:ident)+) $($argument:tt)*)) => {
        brucket_ast::Node::Application(brucket_ast::Application::new(
            brucket_ast::NodeId(0),
            Box::new(brucket!(@complex_identifier $identifier $($remaining)+)),
            vec![$(brucket!($argument),)*],
        ))
    };
    (($identifier:ident $($argument:tt)*)) => {
        brucket_ast::Node::Application(brucket_ast::Application::new(
            brucket_ast::NodeId(0),
            Box::new(brucket!($identifier)),
            vec![$(brucket!($argument),)*],
        ))
    };

    ($constant_value:literal) => {
        brucket!((0: $constant_value))
    };
    (($node_id:literal: $constant_value:literal)) => {
        brucket_ast::Node::ConstantValue(brucket_ast::constant_value::ConstantValue::new(
            brucket_ast::NodeId($node_id),
            brucket_ast::constant_value::ConstantVariant::from($constant_value),
        ))
    };

    ($identifier:ident$(::$remaining:ident)+) => {
        brucket!(@complex_identifier $identifier $($remaining)+)
    };
    (@complex_identifier $($identifier:ident)+) => {
        brucket_ast::Node::Identifier(brucket_ast::Identifier::new(
            brucket_ast::NodeId(0),
            brucket_ast::path::Path::Complex(
                vec![$(stringify!($identifier).to_owned(),)+],
            )
        ))
    };

    ($identifier:ident) => {
        brucket!((0: $identifier))
    };
    (($node_id:literal: $identifier:ident)) => {
        brucket_ast::Node::Identifier(brucket_ast::Identifier::new(
            brucket_ast::NodeId($node_id),
            brucket_ast::path::Path::Simple(stringify!($identifier).to_owned()),
        ))
    };

    (($identifier:tt $($argument:tt)*)) => {
        brucket_ast::Node::Application(brucket_ast::Application::new(
            brucket_ast::NodeId(0),
            Box::new(brucket!($identifier)),
            vec![$(brucket!($argument),)*],
        ))
    };
}
