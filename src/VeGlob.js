const VeRegex = require('./VeRegex.js')

// :: (char) -> bool
const isalpha = (c) => {
    return (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
}

class VeGlob {
    constructor(glob) {
        this._glob = glob
        this._re = this._regex(glob)
        this._Regex = new VeRegex(this._re)
    }

    // :: (string) -> bool
    test(text) {
        return this._Regex.test(text)
    }

    // :: (string) -> string
    _regex(wc) {
        const re = []
        for (let i = 0; i < wc.length; i++) {
            switch(wc[i]) {
            case '?':
                re.push('.')
                break
            case '*':
                re.push('.*')
                break
            // TODO
            //case '.':
            //    re.push('\.')
            //    break
            default:
                if (isalpha(wc[i])) {
                    re.push(`[${wc[i].toLowerCase()}${wc[i].toUpperCase()}]`)
                } else {
                    re.push(wc[i])
                }
                break
            }
        }
        return re.join('')
    }
}

module.exports = VeGlob
