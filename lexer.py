# pip install lark-parser click
from lark import Lark
import tokenize as tk
import click
import os


GRAMMARS = {
    "elm": r"""
    start : (OP | NAME | STRING | NUMBER)+

    OP : /[()[\]{}:,<>+*\/\-=]|==|\/=|<=|>=|..|->|\+\+/
    NAME : /(?!\d)[\w_]+/
    STRING : /"[^"\n]*"/
    NUMBER : /\d+(\.\d+)?([eE][+-]?\d+)?/

    %ignore /\s+/
    %ignore /--[^\n]*/
    """,

    "haskell": r"""
    start : (OP | NAME | STRING | NUMBER)+

    OP : /[()[\]{},<>+*\/\-=|.`]|==|\/=|<=|>=|->|::|\+\+/
    NAME : /(?!\d)[\w_]+/
    STRING : /"[^"\n]*"/
    NUMBER : /\d+(\.\d+)?([eE][+-]?\d+)?/

    %ignore /\s+/
    %ignore /--[^\n]*/
    """,

    "typescript": r"""
    start : (OP | NAME | STRING | NUMBER)+

    OP : /==|!=|<=|>=|&&|\?\.|\|\||[()[\]{}:,<>+*\/\-;|=\.!?]/
    NAME : /(?!\d)[\w_]+/
    STRING : /"[^"\n]*"|'[^'\n]*'|`[^`\n]*`/
    NUMBER : /\d+(\.\d+)?([eE][+-]?\d+)?/

    %ignore /\s+/
    %ignore /\/\/[^\n]*/
    """,

    "c++": r"""
    start : (OP | NAME | STRING | NUMBER)+

    OP : /==|!=|<=|>=|&&|->|\#include|\#define|[()[\]{}:,<>+*\/\-;|=\.!?]/
    NAME : /(?!\d)[\w_]+/
    STRING : /"[^"\n]*"|'[^'\n]*'|`[^`\n]*`/
    NUMBER : /\d+(\.\d+)?([eE][+-]?\d+)?/

    %ignore /\s+/
    %ignore /\/\/[^\n]*/
    """,
}

EXTENSIONS = {
    "elm": "elm",
    "ts": "typescript",
    "cpp": "c++",
    "h": "c++",
    "hs": "haskell",
    "py": "python",
}


@click.command()
@click.argument("path")
@click.option("--lang", "-l", help="Language type")
@click.option("--verbose", "-v", is_flag=True, help="Language type")
@click.option("--token", "-t", help="Token type")
def main(path, lang, verbose, token):
    if lang is None:
        ext = os.path.splitext(path)[-1].strip(".")
        grammar = GRAMMARS[EXTENSIONS[ext]]
    else:
        grammar = GRAMMARS[lang]

    with open(path) as fd:
        src = fd.read()
        stream = list(Lark(grammar, lexer="standard").lex(src))

    if verbose:
        for tk in stream:
            if token is None or (tk.type == token):
                print(f"    {tk!r}")

    print(f"Tokens count: {len(stream)}")


if __name__ == "__main__":
    main()
