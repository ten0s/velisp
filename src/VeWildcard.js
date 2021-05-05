const VeRegex = require('./VeRegex.js')

class VeWildcard {
    constructor(wc) {
        this.wc = wc
        this.re = this.toRegex(wc)
        //console.log(this.re)
        this.Regex = new VeRegex(this.re)
    }

    // :: (string) -> bool
    test(text) {
        return this.Regex.test(text)
    }

    // :: (string) -> string
    toRegex(wc) {
        const re = []
        for (let i = 0; i < wc.length; i++) {
            switch(wc[i]) {
            case '#':
                re.push('[0123456789]')
                break
            case '@':
                re.push('[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]')
                break
            case '.':
                re.push('[^0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789]')
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
}

module.exports = VeWildcard
