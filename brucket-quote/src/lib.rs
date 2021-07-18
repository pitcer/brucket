/*
 * MIT License
 *
 * Copyright (c) 2020 Piotr Dobiech
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#![forbid(unsafe_code)]

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
        Module::new(
            NodeId($node_id),
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
        Constant::new(
            NodeId($node_id),
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
        InternalFunction::new(
            NodeId($node_id),
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
        Function::new(
            NodeId($node_id),
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
        Node::$node_type(brucket!($node))
    };

    (@visibility public) => { Visibility::Public };
    (@visibility private) => { Visibility::Private };

    (@strategy eager) => { ApplicationStrategy::Eager };
    (@strategy lazy) => { ApplicationStrategy::Lazy };

    ((lambda $parameters:tt $body:tt)) => {
        Node::Lambda(brucket!(@lambda 0 $parameters any $body))
    };
    (($node_id:literal: lambda $parameters:tt -> $return_type:tt $body:tt)) => {
        Node::Lambda(brucket!(@lambda $node_id $parameters $return_type $body))
    };
    (@lambda $node_id:literal $parameters:tt $return_type:tt $body:tt) => {
        Lambda::new(
            NodeId($node_id),
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
        Parameter::new(
            stringify!($name).to_owned(),
            brucket!(@type $type),
            Arity::Variadic
        )
    };
    (@parameter ($name:ident: $type:tt)) => {
        Parameter::new(
            stringify!($name).to_owned(),
            brucket!(@type $type),
            Arity::Unary
        )
    };

    ((let $name:ident $value:tt $then:tt)) => {
        brucket!((let $name: any $value $then))
    };
    ((let $name:ident: $value_type:tt $value:tt $then:tt)) => {
        brucket!((0: let $name: $value_type $value $then))
    };
    (($node_id:literal: let $name:ident: $value_type:tt $value:tt $then:tt)) => {
        Node::Let(Let::new(
            NodeId($node_id),
            stringify!($name).to_owned(),
            brucket!(@type $value_type),
            Box::new(brucket!($value)),
            Box::new(brucket!($then)),
        ))
    };

    (@type any) => { Type::Any };
    (@type unit) => { Type::Unit };
    (@type bool) => { Type::Boolean };
    (@type int) => { Type::Integer };
    (@type float) => { Type::Float };
    (@type str) => { Type::String };
    (@type ($($parameter_type:ident)* -> $return_type:ident)) => {
        brucket!(@type (($($parameter_type)*) -> $return_type))
    };
    (@type (($($parameter_type:tt)*) -> $return_type:ident)) => {
        Type::Lambda(LambdaType::new(
            vec![$(brucket!(@type $parameter_type),)*],
            Box::new(brucket!(@type $return_type))
        ))
    };
    (@type $symbol:ident) => {
        Type::Symbol(stringify!($symbol).to_owned())
    };

    ((if $condition:tt $if_true:tt $if_false:tt)) => {
        brucket!((0: if $condition $if_true $if_false))
    };
    (($node_id:literal: if $condition:tt $if_true:tt $if_false:tt)) => {
        Node::If(If::new(
            NodeId($node_id),
            Box::new(brucket!($condition)),
            Box::new(brucket!($if_true)),
            Box::new(brucket!($if_false)),
        ))
    };

    (()) => {
        Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Unit,
        ))
    };

    (null) => {
        Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Null,
        ))
    };

    ((($identifier:ident$(::$remaining:ident)+) $($argument:tt)*)) => {
        Node::Application(Application::new(
            NodeId(0),
            Box::new(brucket!(@complex_identifier $identifier $($remaining)+)),
            vec![$(brucket!($argument),)*],
        ))
    };
    (($identifier:ident $($argument:tt)*)) => {
        Node::Application(Application::new(
            NodeId(0),
            Box::new(brucket!($identifier)),
            vec![$(brucket!($argument),)*],
        ))
    };

    ($constant_value:literal) => {
        brucket!((0: $constant_value))
    };
    (($node_id:literal: $constant_value:literal)) => {
        Node::ConstantValue(ConstantValue::new(
            NodeId($node_id),
            ConstantVariant::from($constant_value),
        ))
    };

    ($identifier:ident$(::$remaining:ident)+) => {
        brucket!(@complex_identifier $identifier $($remaining)+)
    };
    (@complex_identifier $($identifier:ident)+) => {
        Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Complex(
                vec![$(stringify!($identifier).to_owned(),)+],
            )
        ))
    };

    ($identifier:ident) => {
        brucket!((0: $identifier))
    };
    (($node_id:literal: $identifier:ident)) => {
        Node::Identifier(Identifier::new(
            NodeId($node_id),
            Path::Simple(stringify!($identifier).to_owned()),
        ))
    };

    (($identifier:tt $($argument:tt)*)) => {
        Node::Application(Application::new(
            NodeId(0),
            Box::new(brucket!($identifier)),
            vec![$(brucket!($argument),)*],
        ))
    };
}
