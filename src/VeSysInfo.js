import {readFileSync} from 'fs'
import {join} from 'path'
import __rootdir from './VeRootDir.js'

const config = JSON.parse(readFileSync(join(__rootdir, 'package.json')))

const VeSysInfo = {
    isRepl: false,
    name: config.name,
    version: config.version,
}

export default VeSysInfo
