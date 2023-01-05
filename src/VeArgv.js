/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2023 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

/* SPDX-License-Identifier: GPL-3.0-or-later */

const opts = [
    {short: '-e', full: '--eval'  , arg: '<expr>' , default: undefined, help: 'evaluate script'},
    {short: ''  , full: '--no-dcl', arg: undefined, default: true     , help: 'run without dcl'},
]

// :: () -> [{option, default, help}]
const options = () => {
    return opts.map(o => {
        const full = o.full ? (o.arg ? `${o.full} ${o.arg}` : o.full) : ''
        const option = o.short ? `${o.short}, ${full}` : full
        return {
            option,
            default: o.default,
            help: o.help,
        }
    })
}

// :: ([string]) -> [[string], [string]
const parseArgv = (argv) => {
    let init = [...argv]
    let rest = []
    const i = argv.indexOf('-')
    const j = argv.indexOf('--')
    const k = i !== -1 ? i : j
    if (k !== -1) {
        init = argv.slice(0, k)
        rest = argv.slice(k)
    }
    return [init, rest]
}

// :: ([string]) -> [string]
const initArgv = (argv) => {
    const [init, ] = parseArgv(argv)
    return init
}

// Remove main.js specific options
// :: ([string]) -> [string]
const removeOpts = (argv) => {
    const out = []

    for (let i = 0; i < argv.length; i++) {
        const arg = argv[i]

        let keep = true
        for (const opt of opts) {
            if (opt.short && !opt.arg) {
                // -s
                if (arg === opt.short) {
                    keep = false
                    break
                }
            }

            if (opt.full && !opt.arg) {
                // --full
                if (arg === opt.full) {
                    keep = false
                    break
                }
            }

            if (opt.short && opt.arg) {
                // -s SPACE arg
                if (arg === opt.short) {
                    keep = false
                    i++ // skip next arg
                    break
                }
                // --s=arg
                if (arg.startsWith(opt.short + '=')) {
                    keep = false
                    break
                }
            }

            if (opt.full && opt.arg) {
                // --full SPACE arg
                if (arg === opt.full) {
                    keep = false
                    i++ // skip next arg
                    break
                }
                // --full=arg
                if (arg.startsWith(opt.full + '=')) {
                    keep = false
                    break
                }
            }
        }

        if (keep) {
            out.push(arg)
        }
    }

    return out
}

// devel mode  : $ node src/main.js [--no-dcl] test.js 1 two
// ["node", "src/main.js"]
// release mode: $ velisp [--no-dcl] test.js 1 two
// ["velisp", "/snapshot/velisp/pkg/src/main.js"]

// :: ([string]) => [string]
const lspArgv0 = (argv) => {
    // It's oversimplification here, since
    // [.../]node [../]src/main.js may not be enough
    // shortly. For example we may need to pass --stack-size, etc
    // options, but it works for a while.
    // See lspArgv also.
    return [...argv].slice(0, 2)
}

// -/-- is expected to be used when no file name is given, i.e.
// in stdin, tty and eval modes
// devel mode  : $ node src/main.js [--no-dcl] test.js 1 two
// release mode: $ velisp [--no-dcl] test.js 1 two
// ["test.js", "1", "two"]
// stdin mode  : $ velisp [--no-dcl] -- 1 two
// tty mode    : $ cat test.js | velisp [--no-dcl] -- 1 two
// eval mode   : $ velisp [--no-dcl] --eval '(ver)' -- 1 two
// ["--", "1", "two"]

// :: ([string]) -> [string]
const lspArgv = (argv) => {
    const [, rest] = parseArgv(argv)
    // The user forced args by passing -/--
    if (rest.length) {
        return rest
    }
    // Skip node, src/main.js and
    // remove main.js specific options.
    // See lspArgv0 for detail.
    return removeOpts([...argv].slice(2))
}

export default {
    options,
    parseArgv,
    initArgv,
    lspArgv0,
    lspArgv,
}
