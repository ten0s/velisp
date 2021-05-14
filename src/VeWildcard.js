const VeRegex = require('./VeRegex.js')

class VeWildcard {
    constructor(wc) {
        this._wcs = this._splitByComma(wc)
        this._res = this._wcs.map(wc => this._regex(wc))
        this._Regexs = this._res.map(re => new VeRegex(re))
    }

    // :: (string) -> bool
    test(text) {
        return this._Regexs.some(Regex => Regex.test(text))
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
            case '~':
                if (wc[i-1] === '[') { // [~
                    re.push('^')
                } else {
                    re.push('~')
                }
                break
            case '^': // literal ^
                re.push('\\^')
                break
            case '`': // escape
                re.push('\\')
                break
            default:
                re.push(wc[i])
                break
            }
        }
        return re.join('')
    }

    // :: (string) -> [string]
    _splitByComma(str) {
        const out = []
        const arr = Array.from(str)
        let acc = []
        let insideGroup = false
        for (let i = 0; i < arr.length; i++) {
            // TODO: support cases like [\]\[]
            if (arr[i] === '[') {
                insideGroup = true
            }
            if (arr[i] === ']') {
                insideGroup = false
            }
            if (arr[i] === ',' && !insideGroup && arr[i-1] !== '`') {
                out.push(acc.join(''))
                acc = []
            } else {
                acc.push(arr[i])
            }
        }
        out.push(acc.join(''))
        return out
    }

    // :: () -> string
    toRegex() {
        return this._res.join(' <|OR|> ')
    }

    // :: () -> string
    toDot() {
        return this._Regexs.map(Regex => Regex.toDot()).join('\n')
    }
}

module.exports = VeWildcard
