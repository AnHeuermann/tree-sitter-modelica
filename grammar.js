/*
 * OMFrontend.js
 * Copyright (C) 2022 Perpetual Labs, Ltd.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * @author Mohamad Omar Nachawati <omar@perpetuallabs.io>
 */

module.exports = grammar({
    name: "modelica",
    //conflicts: $ => [
    //    [$.component_list],
    //    [$.equation_list],
    //    [$.functionarguments]
    //],
    extras: $ => [
        $.comment,
        $.BLOCK_COMMENT,
        $._SPACE
    ],
    word: $ => $.IDENT,
    rules: {

        // A.2.1 Stored Definition – Within

        stored_definition: $ => seq(
            optional($.BOM),
            optional(
                seq(
                    "within",
                    optional(field("name", $.name)),
                    ";")
            ),
            repeat(
                seq(
                    optional(field("final", "final")),
                    field("classDefinition", $.class_definition),
                    ";"
                )
            )
        ),

        // A.2.2 Class Definition

        class_definition: $ => seq(
            optional(field("encapsulated", "encapsulated")),
            field("classPrefixes", $.class_prefixes),
            field("classSpecifier", $.class_specifier)
        ),

        class_prefixes: $ => seq(
            optional(field("partial", "partial")),
            choice(
                field("class", "class"),
                field("model", "model"),
                seq(
                    optional(field("operator", "operator")),
                    field("record", "record")),
                field("block", "block"),
                seq(
                    optional(field("expandable", "expandable")),
                    field("connector", "connector")),
                field("type", "type"),
                field("package", "package"),
                seq(
                    optional(choice(field("pure", "pure"), field("impure", "impure"))),
                    optional(field("operator", "operator")),
                    field("function", "function")),
                field("operator", "operator")
            )
        ),

        class_specifier: $ => choice(
            $.long_class_specifier,
            $.short_class_specifier,
            $.der_class_specifier
        ),

        long_class_specifier: $ => choice(
            seq(
                field("identifier", $.IDENT),
                optional(field("descriptionString", $.description_string)),
                field("composition",
                    seq(
                        optional($.element_list),
                        repeat(
                            choice(
                                seq("public", optional($.element_list)),
                                seq("protected", optional($.element_list)),
                                $.equation_section,
                                $.algorithm_section
                            )
                        ),
                        optional(
                            seq(
                                "external",
                                $.language_specification
                            )
                        ),
                        optional(
                            seq(
                                $.external_function_call,
                                $.annotation_clause,
                                ";"
                            )
                        ),
                        optional(
                            seq(
                                $.annotation_clause,
                                ";"
                            )
                        )
                    ),
                ),
                "end",
                field("endIdentifier", $.IDENT)
            ),
            seq(
                "extends",
                field("identifier", $.IDENT),
                field("classModification", $.class_modification),
                optional(field("descriptionString", $.description_string)),
                field("composition",
                    seq(
                        optional($.element_list),
                        repeat(
                            choice(
                                seq("public", optional($.element_list)),
                                seq("protected", optional($.element_list)),
                                $.equation_section,
                                $.algorithm_section
                            )
                        ),
                        optional(
                            seq(
                                "external",
                                $.language_specification
                            )
                        ),
                        optional(
                            seq(
                                $.external_function_call,
                                $.annotation_clause,
                                ";"
                            )
                        ),
                        optional(
                            seq(
                                $.annotation_clause,
                                ";"
                            )
                        )
                    ),
                ),
                "end",
                field("endIdentifier", $.IDENT)
            )
        ),

        short_class_specifier: $ => choice(
            seq(
                field("identifier", $.IDENT),
                "=",
                optional(field("basePrefix", $.base_prefix)), // TODO: Is this optional?
                field("typeSpecifier", $.type_specifier),
                optional(field("subscripts", $.array_subscripts)),
                optional(field("classModification", $.class_modification)),
                optional(field("descriptionString", $.description_string)),
            ),
            seq(
                field("identifier", $.IDENT),
                "=",
                "enumeration",
                "(",
                choice(
                    $.enum_list,
                    ":"
                ),
                ")",
                optional(field("descriptionString", $.description_string)),
                optional(field("annotationClause", $.annotation_clause))
            ),
        ),

        der_class_specifier: $ => seq(
            field("identifier", $.IDENT),
            "=",
            "der",
            "(",
            field("typeSpecifier", $.type_specifier),
            ",",
            field("argument", $.IDENT),
            optional(seq(",", field("argument", $.IDENT))), ")",
            optional(field("descriptionString", $.description_string)),
            optional(field("annotationClause", $.annotation_clause))
        ),

        base_prefix: $ => choice(
            "input",
            "output"
        ),

        enum_list: $ => seq(
            field("enumerationLiteral", $.enumeration_literal),
            repeat(seq(",", field("enumerationLiteral", $.enumeration_literal)))
        ),

        enumeration_literal: $ => seq(
            field("identifier", $.IDENT),
            field("description", seq(
                optional(field("descriptionString", $.description_string)),
                optional(field("annotationClause", $.annotation_clause))
                )
            )
        ),

        // composition can be empty, so it's integrated in long_class_specifier

        language_specification: $ => field(
            "value", $.STRING
        ),

        external_function_call: $ => seq(
            optional(seq(
                field("componentReference", $.component_reference),
                "="
            )),
            field("identifier", $.IDENT),
            "(",
            optional(field("expressions", $.expression_list)), ")"
        ),

        // element-list can be empty, so it's repeat1 and every location has to use optional($.element_list)
        element_list: $ => repeat1(
            seq(field("element", $.element), ";")
        ),

        element: $ => choice(
            $.import_clause,
            $.extends_clause,
            $._named_element
        ),

        _named_element: $ => seq(
            optional(field("redeclare", "redeclare")),
            optional(field("final", "final")),
            optional(field("inner", "inner")),
            optional(field("outer", "outer")),
            choice(
                field("classDefinition", $.class_definition),
                field("componentClause", $.component_clause),
                seq(
                    "replaceable",
                    choice(
                        field("classDefinition", $.class_definition),
                        field("componentClause", $.component_clause)
                    ),
                    optional(
                        field("constrainingClause", $.constraining_clause)
                    ),
                )
            )
        ),

        import_clause: $ => seq(
            "import",
            choice(
                seq(
                    field("alias", $.IDENT),
                    "=",
                    field("name", $.name)),
                seq(
                    field("name", $.name),
                    optional(
                        //field("wildcard", ".*"),
                        seq(
                            ".",
                            choice(
                                field("wildcard", "*"),
                                seq("{", field("imports", $.import_list), "}")
                            )
                        )
                    )
                )
            ),
            field("description", seq(
                optional(field("descriptionString", $.description_string)),
                optional(field("annotationClause", $.annotation_clause))
            ))
        ),

        import_list: $ => seq(
            field("import", $.IDENT),
            repeat(seq(",", field("import", $.IDENT)))
        ),

        // A.2.3 Extends

        extends_clause: $ => seq(
            "extends",
            field("typeSpecifier", $.type_specifier),
            optional(field("classModification", $.class_modification)),
            optional(field("annotationClause", $.annotation_clause)),
        ),

        constraining_clause: $ => seq(
            "constrainedby",
            field("typeSpecifier", $.type_specifier),
            optional(field("classModification", $.class_modification))
        ),

        // A.2.4 Component Clause

        component_clause: $ => seq(
            field("typePrefix",
                seq(
                optional(
                    choice(
                        field("flow", "flow"),
                        field("stream", "stream"))),
                optional(choice(
                    field("discrete", "discrete"),
                    field("parameter", "parameter"),
                    field("constant", "constant"))),
                optional(choice(
                    field("input", "input"),
                    field("output", "output"))),
            )),
            field("typeSpecifier", $.type_specifier),
            optional(field("subscripts", $.array_subscripts)),
            field("componentDeclarations", $.component_list)
        ),

        // type-prefix can be empty, so it is integrated into component_clause

        component_list: $ => seq(
            field("componentDeclaration", $.component_declaration),
            repeat(seq(",", field("componentDeclaration", $.component_declaration)))
        ),

        component_declaration: $ => seq(
            field("declaration", $.declaration),
            optional(field("conditionAttribute", $.condition_attribute)),
            field("description", seq(
                optional(field("descriptionString", $.description_string)),
                optional(field("annotationClause", $.annotation_clause))
            ))
        ),

        condition_attribute: $ => seq(
            "if",
            $.expression
        ),

        declaration: $ => seq(
            field("identifier", $.IDENT),
            optional(field("subscripts", $.array_subscripts)),
            optional(field("modification", $.modification))
        ),

        // A.2.5 Modification

        modification: $ => choice(
            seq(
                field("classModification", $.class_modification),
                optional(seq("=", field("expression", $.expression)))
            ),
            seq(choice("=", ":="), field("expression", $.expression))
        ),

        class_modification: $ => seq(
            "(", optional(field("arguments", $.argument_list)), ")"
        ),

        argument_list: $ => seq(
            field("argument", $.argument),
            repeat(seq(",", field("argument", $.argument)))
        ),

        argument: $ => choice(
            $.element_modification_or_replaceable,
            $.element_redeclaration
        ),

        element_modification_or_replaceable: $ => seq(
            optional(field("each", "each")),
            optional(field("final", "final")),
            choice(
                field("modification", $.element_modification),
                field("replaceable", $.element_replaceable)
            )
        ),

        element_modification: $ => seq(
            field("name", $.name),
            optional(field("modification", $.modification)),
            optional(field("descriptionString", $.description_string))
        ),

        element_redeclaration: $ => seq(
            field("redeclare", "redeclare"),
            optional(field("each", "each")),
            optional(field("final", "final")),
            choice(
                field("shortClassDefinition", $.short_class_definition),
                field("componentClause1", $.component_clause1),
                field("elementReplaceable", $.element_replaceable),
            )
        ),

        element_replaceable: $ => seq(
            field("replaceable", "replaceable"),
            choice(
                field("shortClassDefinition", $.short_class_definition),
                field("componentClause1", $.component_clause1)
            ),
            optional(field("constrainingClause", $.constraining_clause))
        ),

        component_clause1: $ => seq(
            field("typePrefix",
                seq(
                optional(
                    choice(
                        field("flow", "flow"),
                        field("stream", "stream"))),
                optional(choice(
                    field("discrete", "discrete"),
                    field("parameter", "parameter"),
                    field("constant", "constant"))),
                optional(choice(
                    field("input", "input"),
                    field("output", "output"))),
            )),
            field("typeSpecifier", $.type_specifier),
            field("componentDeclaration1", $.component_declaration1),
        ),

        component_declaration1: $ => seq(
            field("declaration", $.declaration),
            field("description", seq(
                optional(field("descriptionString", $.description_string)),
                optional(field("annotationClause", $.annotation_clause))
            ))
        ),

        short_class_definition: $ => seq(
            field("classPrefixes", $.class_prefixes),
            field("classSpecifier", $.short_class_specifier)
        ),

        // A.2.6 Equations

        equation_section: $ => prec.left(seq(
            optional(field("initial", "initial")),
            "equation",
            field("equations", repeat(seq(field("equation", $.equation), ";")))
        )),

        algorithm_section: $ =>  prec.right(seq(
            optional(field("initial", "initial")),
            "algorithm",
            field("statements", repeat(seq(field("statement", $.statement), ";")))
        )),

        equation: $ => seq(
            choice(
                seq($.simple_expression, "=", $.expression),
                $.if_equation,
                $.for_equation,
                $.connect_clause,
                $.when_equation,
                seq($.component_reference, $.function_call_args)
            ),
            field("description", seq(
                optional(field("descriptionString", $.description_string)),
                optional(field("annotationClause", $.annotation_clause))
            ))
        ),

        statement: $ => seq(
            choice(
                seq(
                    field("componentReference", $.component_reference),
                    choice(
                        seq(":=", field("sourceExpression", $.expression)),
                        field("functionCallArgs", $.function_call_args)
                    )
                ),
                seq(
                    "(",
                    optional(field("expressions", $.output_expression_list)),
                    ")",
                    ":=",
                    field("componentReference", $.component_reference),
                    field("functionCallArgs", $.function_call_args)
                ),
                "break",
                "return",
                $.if_statement,
                $.for_statement,
                $.while_statement,
                $.when_statement
            ),
            field("description", seq(
                optional(field("descriptionString", $.description_string)),
                optional(field("annotationClause", $.annotation_clause))
            ))
        ),

        if_equation: $ => seq(
            "if",
            field("condition", $.expression),
            "then",
            optional(field("thenEquations", repeat(seq(field("equation", $.equation), ";")))),
            repeat(seq(
                "elseif",
                field("elseIfCondition", $.expression),
                "then",
                optional(field("thenEquations", prec.left(repeat(seq(field("equation", $.equation), ";"))))),
            )),
            optional(seq(
                "else",
                optional(field("thenEquations", prec.left(repeat(seq(field("equation", $.equation), ";"))))),
            )),
            "end", "if"
        ),

        if_statement: $ => seq(
            "if",
            field("condition", $.expression),
            "then",
            optional(field("thenStatements", repeat(seq(field("statement", $.statement), ";")))),
            repeat(seq(
                "elseif",
                field("elseIfCondition", $.expression),
                "then",
                optional(field("thenStatements", prec.left(repeat(seq(field("statement", $.statement), ";"))))),
            )),
            optional(seq(
                "else",
                optional(field("thenStatements", prec.left(repeat(seq(field("statement", $.statement), ";"))))),
            )),
            "end", "if"
        ),

        for_equation: $ => seq(
            "for",
            field("indices", $.for_indices),
            "loop",
            field("equations", repeat(seq(field("equation", $.equation), ";"))),
            "end", "for",
        ),

        for_statement: $ => seq(
            "for",
            field("indices", $.for_indices),
            "loop",
            field("statements", repeat(seq(field("statement", $.statement), ";"))),
            "end", "for",
        ),

        for_indices: $ => seq(
            field("index", $.for_index),
            repeat(seq(",", field("index", $.for_index)))
        ),

        for_index: $ => seq(
            field("identifier", $.IDENT),
            optional(seq(
                "in",
                field("expression", $.expression)))
        ),

        while_statement: $ => seq(
            "while",
            field("condition", $.expression),
            "loop",
            field("statements", repeat(seq(field("statement", $.statement), ";"))),
            "end", "while"
        ),

        when_equation: $ => seq(
            "when",
            field("condition", $.expression),
            "then",
            field("equations", repeat(seq(field("equation", $.equation), ";"))),
            repeat(seq(
                "elsewhen",
                field("elsewenCondition", $.expression),
                "then",
                optional(field("thenEquations", repeat(seq(field("equation", $.equation), ";")))),
            )),
            "end", "when"
        ),

        when_statement: $ => seq(
            "when",
            field("condition", $.expression),
            "then",
            field("statements", repeat(seq(field("statement", $.statement), ";"))),
            repeat(seq(
                "elsewhen",
                field("elsewenCondition", $.expression),
                "then",
                optional(field("thenstatements", repeat(seq(field("statement", $.statement), ";")))),
            )),
            "end", "when"
        ),

        connect_clause: $ => seq(
            "connect",
            "(",
            field("component1", $.component_reference),
            ",",
            field("component2", $.component_reference),
            ")"
        ),

        // A.2.7 Expressions

        expression: $ => choice(
            $.simple_expression,
            $._if_expression
        ),

        _if_expression: $ => seq(
            "if", field("condition", $.expression),
            "then", field("thenExpression", $.expression),
            repeat(field("elseIfClause", $._else_if_clause)),
            "else", field("elseExpression", $.expression)
        ),

        _else_if_clause: $ => seq(
            "elseif", field("condition", $.expression),
            "then", field("expression", $.expression)
        ),

        simple_expression: $ => seq(
            $.logical_expression,
            optional(seq(
                ":",
                $.logical_expression,
                optional(seq(
                    ":",
                    $.logical_expression,
                ))
            ))
        ),

        logical_expression: $ => seq(
            $.logical_term,
            repeat(seq(
                "or",
                $.logical_term
            ))
        ),

        logical_term: $ => seq(
            $.logical_factor,
            repeat(seq(
                "and",
                $.logical_factor
            ))
        ),

        logical_factor: $ => seq(
            optional("not"),
            $.relation
        ),

        relation: $ => seq(
            $.arithmetic_expression,
            optional(seq(
                $.relational_operator,
                $.arithmetic_expression
            ))
        ),

        relational_operator: $ => choice(
            "<",
            "<=",
            ">",
            ">=",
            "==",
            "<>"
        ),

        arithmetic_expression: $ => seq(
            optional($.add_operator),
            $.term,
            repeat(seq(
                $.add_operator,
                $.term
            ))
        ),

        add_operator: $ => choice(
            "+",
            "-",
            ".+",
            ".-"
        ),

        term: $ => seq(
            $.factor,
            repeat(seq(
                $.mul_operator,
                $.factor
            ))
        ),

        mul_operator: $ => choice(
            "*",
            "/",
            ".*",
            "./"
        ),

        factor: $ => seq(
            $.primary,
            repeat(seq(
                choice("^", ".^"),
                $.primary
            ))
        ),

        primary: $ => choice(
            $.UNSIGNED_NUMBER,
            $.STRING,
            "false",
            "true",
            seq(
                choice(
                    $.component_reference,
                    "der",
                    "initial",
                    "pure"
                ),
                $.function_call_args
            ),
            $.component_reference,
            seq(
                "(",
                optional(field("expressions", $.output_expression_list)),
                ")"
            ),
            seq(
                "[",
                $.expression_list,
                repeat(seq(";", field("expressions", $.expression_list))),
                "]"
            ),
            seq(
                "{",
                $.array_arguments,
                "}"
            ),
            "end"
        ),

        UNSIGNED_NUMBER: $ => choice(
            $.UNSIGNED_INTEGER,
            $.UNSIGNED_REAL
        ),

        type_specifier: $ => seq(
            optional(field("global", ".")),
            field("name", $.name)
        ),

        name: $ => prec.left(choice(
            seq(field("qualifier", $.name), ".", field("identifier", $.IDENT)),
            field("identifier", $.IDENT)
        )),

        component_reference: $ => prec.left(choice(
            seq(
                field("qualifier", $.component_reference), ".", field("identifier", $.IDENT), optional(field("subscripts", $.array_subscripts))
            ),
            seq(
                optional(field("global", ".")), field("identifier", $.IDENT), optional(field("subscripts", $.array_subscripts))
            )
        )),

        function_call_args: $ => seq(
            "(",
            field("arguments", $.function_arguments),
            ")"
        ),

        function_arguments: $ => choice(
            seq(
                $.expression,
                optional(
                    choice(
                        seq(".", $.function_arguments_non_first),
                        seq("for", $.for_indices)
                ))
            ),
            seq(
                $.function_partial_application,
                optional(seq(",", $.function_arguments_non_first))
            ),
            $.named_arguments
        ),

        function_arguments_non_first: $ => choice(
            seq(
                $.function_arguments,
                optional(seq(
                    ",",
                    $.function_arguments_non_first
                ))
            ),
            $.named_arguments
        ),

        array_arguments: $ => seq(
            field("argument", $.expression),
            optional(choice(
                seq(",", $.array_arguments_non_first),
                seq("for", $.for_indices)
            ))
        ),

        array_arguments_non_first: $ => seq(
            $.expression,
            optional(seq(
                ",",
                $.array_arguments_non_first
            ))
        ),

        named_arguments: $ => seq(
            field("namedArgument", $.named_argument),
            optional(seq(",", field("namedArguments", $.named_arguments)))
        ),

        named_argument: $ => seq(
            field("identifier", $.IDENT),
            "=",
            $.function_argument
        ),

        function_argument: $ => choice(
            $.function_partial_application,
            $.expression
        ),

        function_partial_application: $ => seq(
            "function",
            field("typeSpecifier", $.type_specifier),
            "(",
            optional(field("namedArguments", $.named_arguments)),
            ")"
        ),

        // Can also be empty, so use optional($.output_expression_list)
        output_expression_list: $ => choice(
            seq(
                field("expression", $.expression),
                repeat(seq(field("comma", ","), optional(field("expression", $.expression))))
            ),
            repeat1(seq(
                field("comma", ","),
                optional(field("expression", $.expression))))
        ),

        expression_list: $ => seq(
            field("expression", $.expression),
            repeat(seq(",", field("expression", $.expression)))
        ),

        array_subscripts: $ => seq(
            "[",
            field("subscript", $.subscript),
            repeat(seq(",", field("subscript", $.subscript))),
            "]"
        ),

        subscript: $ => choice(
            ":",
            field("expression", $.expression)
        ),

        // description can be empty

        description_string: $ => seq(
            field("value", $.STRING), repeat(seq("+", field("value", $.STRING)))
        ),

        annotation_clause: $ => seq(
            "annotation", field("classModification", $.class_modification)
        ),

        // A.1 Lexical conventions

        BOM: $ => /\u00EF\u00BB\u00BF/,

        IDENT: $ => token(choice(
            seq(/[_a-zA-Z]/, repeat(choice(/[0-9]/, /[_a-zA-Z]/))),
            seq("’", repeat(choice(
                /[_a-zA-Z]/, /[0-9]/, "!", "#", "$", "%", "&", "(", ")",
                "*", "+", ",", "-", ".", "/", ":", ";", "<", ">", "=",
                "?", "@", "[", "]", "^", "{", "}", "|", "~", " ", "\"",
                seq("\\", choice("’", "'", "\"", "?", "\\", "a", "b", "f", "n", "r", "t", "v")))), "’"))),

        STRING: $ => token(seq("\"", repeat(choice(/[^"\\]/,
            seq("\\", choice("’", "'", "\"", "?", "\\", "a", "b", "f", "n", "r", "t", "v")))), "\"")),

        UNSIGNED_INTEGER: $ => /[0-9]+/,

        UNSIGNED_REAL: $ => token(choice(
            seq(/[0-9]+/, ".", optional(/[0-9]+/)),
            seq(/[0-9]+/, optional(seq(".", optional(/[0-9]+/))), choice("e", "E"), optional(choice("+", "-")), /[0-9]+/),
            seq(".", /[0-9]+/, optional(seq(choice("e", "E"), optional(choice("+", "-")), /[0-9]+/)))
        )),

        BLOCK_COMMENT: $ => token(
            seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/")
        ),

        comment: $ => token(
            seq("//", /[^\r\n]*/)
        ),

        _SPACE: $ => /\s+/

    }

});
