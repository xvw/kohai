/**
 * @file Rensai is a JSON-like format
 * @author Xavier Van de Woestyne <xaviervdw@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check


function sepBy(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}

module.exports = grammar({
    name: "rensai",

    extras: $ => [/\s/],

    supertypes: $ => [
        $._value,  
    ],

    rules: {
        source_file: $ => $._value,
        _value: $ => choice(
            $.null,
            $.unit,
            $.boolean,
            $.char,
            $.int,
            $.int32,
            $.int64,
            $.float,
            $.string,
            $.pair,
            $.list,
            $.record,
            $.constructor
        ),

        null: _ => 'null',
        unit: _ => '()',
        boolean: _ => choice('true', 'false'),
        int: _ => /-?\d+/,
        int32: _ => /-?\d+l/,
        int64: _ => /-?\d+L/,
        float: _ => /-?\d+\.\d+/,
        
        pair: $ => seq('(', $._value, ',', $._value, ')'),

        list: $ => seq(
            '[',
            optional(sepBy(';', $._value)),
            ']'
        ),

        record: $ => seq(
            '<',
            optional(sepBy(';', $.field)),
            '>'
        ),

        field: $ => seq($.identifier, ':', $._value),

        constructor: $ => seq('#', $.identifier, '(', $._value, ')'),

        identifier: _ => /[a-zA-Z_][a-zA-Z0-9_]*/,

        char: _ => /'[^']'/,
        string: _ => /"([^"\\]|\\.)*"/,
    }
});
