#[cfg(test)]
mod tests;

#[macro_export]
macro_rules! quote {
    ((module $name:ident $functions:tt $internal_functions:tt $constants:tt)) => {
        quote!(@module 0 false $name $functions $internal_functions $constants)
    };
    (($node_id:literal:
        module
        $name:ident
        $functions:tt
        $internal_functions:tt
        $constants:tt
    )) => {
        quote!(@module $node_id false $name $functions $internal_functions $constants)
    };
    (($node_id:literal:
        static
        module
        $name:ident
        $functions:tt
        $internal_functions:tt
        $constants:tt
    )) => {
        quote!(@module $node_id true $name $functions $internal_functions $constants)
    };
    (@module
        $node_id:literal
        $static:literal
        $name:ident
        ($($function:tt)*)
        ($($internal_function:tt)*)
        ($($constant:tt)*)
    ) => {
        $crate::Module::new(
            $crate::NodeId($node_id),
            $static,
            stringify!($name).to_owned(),
            vec![$(quote!($function),)*],
            vec![$(quote!($internal_function),)*],
            vec![$(quote!($constant),)*],
        )
    };

    ((constant $name:ident $value:tt)) => {
        quote!(@constant 0 private $name $value)
    };
    (($node_id:literal: $visibility:ident constant $name:ident $value:tt)) => {
        quote!(@constant $node_id $visibility $name $value)
    };
    (@constant $node_id:literal $visibility:ident $name:ident $value:tt) => {
        $crate::Constant::new(
            $crate::NodeId($node_id),
            quote!(@visibility $visibility),
            stringify!($name).to_owned(),
            quote!($value),
        )
    };

    ((internal_function $name:ident $parameters:tt)) => {
        quote!(@internal_function 0 private eager $name $parameters any)
    };
    (($node_id:literal:
        $visibility:ident
        $strategy:ident
        internal_function
        $name:ident
        $parameters:tt -> $return_type:tt
    )) => {
        quote!(@internal_function $node_id $visibility $strategy $name $parameters $return_type)
    };
    (@internal_function
        $node_id:literal
        $visibility:ident
        $strategy:ident
        $name:ident
        $parameters:tt
        $return_type:tt
    ) => {
        $crate::function::InternalFunction::new(
            $crate::NodeId($node_id),
            quote!(@visibility $visibility),
            quote!(@strategy $strategy),
            stringify!($name).to_owned(),
            quote!(@parameters $parameters),
            quote!(@type $return_type),
        )
    };

    ((function $name:ident $parameters:tt $body:tt)) => {
        quote!(@function 0 private eager $name $parameters any 0 $body)
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
        quote!(
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
        $crate::function::Function::new(
            $crate::NodeId($node_id),
            quote!(@visibility $visibility),
            quote!(@strategy $strategy),
            stringify!($name).to_owned(),
            quote!(@lambda $body_node_id $parameters $return_type $body),
        )
    };

    ((@node $node_type:ident $node:tt)) => {
        quote!(@node $node_type $node)
    };
    (@node ConstantValue $node:tt) => { quote!(@node_unboxed ConstantValue $node) };
    (@node Identifier $node:tt) => { quote!(@node_unboxed Identifier $node) };
    (@node InternalFunction $node:tt) => { quote!(@node_unboxed InternalFunction $node) };
    (@node Module $node:tt) => { quote!(@node_unboxed Module $node) };
    (@node_unboxed $node_type:ident $node:tt) => {
        $crate::Node::$node_type(quote!($node))
    };
    (@node $node_type:ident $node:tt) => {
        $crate::Node::$node_type(Box::new(quote!($node)))
    };

    (@visibility public) => { $crate::Visibility::Public };
    (@visibility private) => { $crate::Visibility::Private };

    (@strategy eager) => { $crate::function::ApplicationStrategy::Eager };
    (@strategy lazy) => { $crate::function::ApplicationStrategy::Lazy };

    ((lambda $parameters:tt $body:tt)) => {
        $crate::Node::Lambda(Box::new(
            quote!(@lambda 0 $parameters any $body)
        ))
    };
    (($node_id:literal: lambda $parameters:tt -> $return_type:tt $body:tt)) => {
        $crate::Node::Lambda(Box::new(
            quote!(@lambda $node_id $parameters $return_type $body)
        ))
    };
    (@lambda $node_id:literal $parameters:tt $return_type:tt $body:tt) => {
        $crate::lambda::Lambda::new(
            $crate::NodeId($node_id),
            quote!(@parameters $parameters),
            quote!(@type $return_type),
            quote!($body),
        )
    };

    (@parameters [$($name:ident)*]) => {
        vec![$(quote!(@parameter ($name: any)),)*]
    };
    (@parameters [$($parameter:tt)*]) => {
        vec![$(quote!(@parameter $parameter),)*]
    };
    (@parameter ($name:ident: $type:tt...)) => {
        $crate::lambda::Parameter::new(
            stringify!($name).to_owned(),
            quote!(@type $type),
            $crate::lambda::Arity::Variadic
        )
    };
    (@parameter ($name:ident: $type:tt)) => {
        $crate::lambda::Parameter::new(
            stringify!($name).to_owned(),
            quote!(@type $type),
            $crate::lambda::Arity::Unary
        )
    };

    ((let $name:ident $value:tt $then:tt)) => {
        quote!((let $name: any $value $then))
    };
    ((let $name:ident: $value_type:tt $value:tt $then:tt)) => {
        quote!((0: let $name: $value_type $value $then))
    };
    (($node_id:literal: let $name:ident: $value_type:tt $value:tt $then:tt)) => {
        $crate::Node::Let(Box::new($crate::Let::new(
            $crate::NodeId($node_id),
            stringify!($name).to_owned(),
            quote!(@type $value_type),
            quote!($value),
            quote!($then),
        )))
    };

    (@type any) => { $crate::ast_type::Type::Any };
    (@type unit) => { $crate::ast_type::Type::Unit };
    (@type bool) => { $crate::ast_type::Type::Boolean };
    (@type int) => { $crate::ast_type::Type::Integer };
    (@type float) => { $crate::ast_type::Type::Float };
    (@type str) => { $crate::ast_type::Type::String };
    (@type ($($parameter_type:ident)* -> $return_type:ident)) => {
        quote!(@type (($($parameter_type)*) -> $return_type))
    };
    (@type (($($parameter_type:tt)*) -> $return_type:ident)) => {
        $crate::ast_type::Type::Lambda(Box::new($crate::ast_type::LambdaType::new(
            vec![$(quote!(@type $parameter_type),)*],
            quote!(@type $return_type)
        )))
    };
    (@type $symbol:ident) => {
        $crate::ast_type::Type::Symbol(stringify!($symbol).to_owned())
    };

    ((if $condition:tt $if_true:tt $if_false:tt)) => {
        quote!((0: if $condition $if_true $if_false))
    };
    (($node_id:literal: if $condition:tt $if_true:tt $if_false:tt)) => {
        $crate::Node::If(Box::new($crate::If::new(
            $crate::NodeId($node_id),
            quote!($condition),
            quote!($if_true),
            quote!($if_false),
        )))
    };

    (()) => {
        $crate::Node::ConstantValue($crate::constant_value::ConstantValue::new(
            $crate::NodeId(0),
            $crate::constant_value::ConstantVariant::Unit,
        ))
    };

    (null) => {
        $crate::Node::ConstantValue($crate::constant_value::ConstantValue::new(
            $crate::NodeId(0),
            $crate::constant_value::ConstantVariant::Null,
        ))
    };

    ((($identifier:ident$(::$remaining:ident)+) $($argument:tt)*)) => {
        $crate::Node::Application(Box::new($crate::Application::new(
            $crate::NodeId(0),
            quote!(@complex_identifier $identifier $($remaining)+),
            vec![$(quote!($argument),)*],
        )))
    };
    (($identifier:ident $($argument:tt)*)) => {
        $crate::Node::Application(Box::new($crate::Application::new(
            $crate::NodeId(0),
            quote!($identifier),
            vec![$(quote!($argument),)*],
        )))
    };

    ($constant_value:literal) => {
        quote!((0: $constant_value))
    };
    (($node_id:literal: $constant_value:literal)) => {
        $crate::Node::ConstantValue($crate::constant_value::ConstantValue::new(
            $crate::NodeId($node_id),
            $crate::constant_value::ConstantVariant::from($constant_value),
        ))
    };

    ($identifier:ident$(::$remaining:ident)+) => {
        quote!(@complex_identifier $identifier $($remaining)+)
    };
    (@complex_identifier $($identifier:ident)+) => {
        $crate::Node::Identifier($crate::Identifier::new(
            $crate::NodeId(0),
            $crate::path::Path::Complex(
                vec![$(stringify!($identifier).to_owned(),)+],
            )
        ))
    };

    ($identifier:ident) => {
        quote!((0: $identifier))
    };
    (($node_id:literal: $identifier:ident)) => {
        $crate::Node::Identifier($crate::Identifier::new(
            $crate::NodeId($node_id),
            $crate::path::Path::Simple(stringify!($identifier).to_owned()),
        ))
    };

    (($identifier:tt $($argument:tt)*)) => {
        $crate::Node::Application(Box::new($crate::Application::new(
            $crate::NodeId(0),
            quote!($identifier),
            vec![$(quote!($argument),)*],
        )))
    };
}
