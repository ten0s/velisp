const opts = [
    {short: '-e', full: '--eval'  , arg: '<expr>' , default: undefined, help: 'evaluate script'},
    {short: ''  , full: '--no-dcl', arg: undefined, default: true     , help: 'run without dcl'},
    {short: ''  , full: '--tree'  , arg: undefined, default: undefined, help: 'show parse tree'},
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
    // remove main.js specific options
    return removeOpts([...argv].slice(2))
}

export default {
    options,
    parseArgv,
    initArgv,
    lspArgv,
}
