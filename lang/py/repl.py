#!/usr/bin/env python3

import code

def custom_repl():
    foo = "bar"
    code.interact(
        banner="sup",
        local=locals()
    )

if __name__ == "__main__":
    custom_repl()
