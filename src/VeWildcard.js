const VeRegex = require('./VeRegex.js')

class VeWildcard {
    constructor(wc) {
        this._wc = wc
        this._re = this._regex(wc)
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
            case '#':
                re.push('[0-9]')
                break
            case '@':
                re.push('[a-zA-Z]')
                break
            case '.':
                re.push('[^a-zA-Z0-9]')
                break
            case '?':
                re.push('.')
                break
            case '*':
                re.push('.*')
                break
            default:
                re.push(wc[i])
                break
            }
        }
        return re.join('')
    }

    // :: () -> string
    toRegex() {
        return this._re
    }

    // :: () -> string
    toDot() {
        return this._Regex.toDot()
    }
}

module.exports = VeWildcard
