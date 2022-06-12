import path from 'path'
import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'
import {homeDir, tmpDir} from '../src/VeSystem.js'

TestRunner.run({
    name: 'vl-filename-mktemp',

    tests: [
        // Test `pattern`
        {test: '(vl-filename-mktemp)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${tmpDir()}${path.sep}velisp-`)
        }},

        {test: '(vl-filename-mktemp "")', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${tmpDir()}${path.sep}velisp-`)
        }},

        {test: '(vl-filename-mktemp "prefix--")', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${tmpDir()}${path.sep}prefix--`)
        }},

        {test: '(vl-filename-mktemp nil)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${tmpDir()}${path.sep}velisp-`)
        }},

        {test: '(vl-filename-mktemp T)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${tmpDir()}${path.sep}velisp-`)
        }},

        {test: '(vl-filename-mktemp nil nil nil)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${tmpDir()}${path.sep}velisp-`)
        }},

        {test: '(vl-filename-mktemp T T T)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${tmpDir()}${path.sep}velisp-`)
        }},

        // Test `directory`
        {test: '(vl-filename-mktemp nil (homedir))', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${homeDir()}${path.sep}velisp-`)
        }},

        // Test `extension`
        {test: '(vl-filename-mktemp nil nil ".suffix")', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().endsWith('.suffix')
        }},

        // Test all args
        {test: '(vl-filename-mktemp "prefix--" (homedir) ".suffix")', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${homeDir()}${path.sep}prefix--`)
                && act.value().endsWith('.suffix')
        }},
    ],

    errors: [
        {test: '(vl-filename-mktemp nil nil nil nil)', result:
         new Error('vl-filename-mktemp: too many arguments')},
        {test: '(vl-filename-mktemp \'pattern)', result:
         new Error('vl-filename-mktemp: `pattern` expected Str, Bool')},
        {test: '(vl-filename-mktemp "" \'directory)', result:
         new Error('vl-filename-mktemp: `directory` expected Str, Bool')},
    ]
})
