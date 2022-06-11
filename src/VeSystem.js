import fs from 'fs'
import os from 'os'

const tmpDir = () => {
    const tmp = process.env['TMP']
    if (tmp && fs.existsSync(tmp)) { return tmp }
    const temp = process.env['TEMP']
    if (tmp && fs.existsSync(temp)) { return temp }
    return os.tmpdir()
}

export {
    tmpDir,
}
