const {Bool, Int, Sym, Str, List, Fun, ensureType} = require('../VeLispTypes.js');
const {VeDclContext} = require('../VeDclContext.js');
const VeDclLoader = require('../VeDclLoader.js');

const util = require('util');

// global dclId index
let _dclId = 0;
const _dclFiles = {};

let _dclDialog = null;

exports.initContext = function (context) {
    context.setSym('ALERT', new Fun('alert', ['string'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('alert: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('alert: too many arguments');
        }
        const str = ensureType('alert:', args[0], [Str]);
        // TODO: Re-implement alert in acad.dcl
        // TODO: Doesn't work reliably w/o parent
        const gi = require('node-gtk');
        const Gtk = gi.require('Gtk', '3.0');
        const dlg = new Gtk.MessageDialog({
            title: "Alert",
            text: str.value(),
            message_type: Gtk.MessageType.INFO,
            buttons: Gtk.ButtonsType.OK
        });
        if (_dclDialog) {
            // FIXME: get rid of internal knowledge
            dlg.transientFor = _dclDialog._gtkWindow;
        }
        dlg.run();
        dlg.destroy();
        return new Bool(false);
    }));
    context.setSym('LOAD_DIALOG', new Fun('load_dialog', ['dclfile'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('load_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('load_dialog: too many arguments');
        }
        const dclFile = ensureType('load_dialog:', args[0], [Str]);
        // TODO: add .dcl extension if not provided
        // TODO: ensure file exists and loads,
        // return a positive integer, or a negative integer on error
        const context = new VeDclContext();

        // Inject lib/dcl/{base,acad}.dcl
        const path = require('path');
        let rootdir = path.join(__dirname, '../..');
        // Win32 workaround
        rootdir = rootdir.split('\\').join('/');
        VeDclLoader.load(`${rootdir}/lib/dcl/base.dcl`, context);
        VeDclLoader.load(`${rootdir}/lib/dcl/acad.dcl`, context);

        const dclDialogs = VeDclLoader.load(dclFile.value(), context);
        console.log(util.inspect(dclDialogs, {showHidden: false, depth: null}));
        const dclMap = {};
        for (const dclDialog of dclDialogs) {
            dclMap[dclDialog.id] = dclDialog;
        }
        const dclId = new Int(_dclId++);
        _dclFiles[dclId.value()] = dclMap;
        return dclId;
    }));
    context.setSym('NEW_DIALOG', new Fun('new_dialog', ['dlg_id', 'dcl_id'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('new_dialog: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('new_dialog: too many arguments');
        }
        const dclId = ensureType('new_dialog: `dcl_id`', args[1], [Int]);
        const dclFile = _dclFiles[dclId.value()];
        if (dclFile) {
            const dlgId = ensureType('new_dialog: `dlg_id`', args[0], [Str]);
            const dclDialog = dclFile[dlgId.value()];
            if (dclDialog) {
                try {
                    _dclDialog = dclDialog.clone();
                    _dclDialog.gtkInitWidget(context);
                    return new Bool(true);
                } catch (e) {
                    // Should never happen since dialog ID is mandatory
                    console.error(e);
                }
            } else {
                // TODO: No dlg_id found in DCL file
            }
        } else {
            // TODO: No dcl_id found
        }
        return new Bool(false);
    }));
    context.setSym('START_DIALOG', new Fun('start_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('start_dialog: too many arguments');
        }
        // TODO: ensure _dclDialog
        const status = _dclDialog.startDialog();
        return new Int(status);
    }));
    context.setSym('DONE_DIALOG', new Fun('done_dialog', ['[status]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('done_dialog: too many arguments');
        }
        let status = 0;
        if (args.length == 1) {
            status = ensureType('done_dialog:', args[0], [Int]);
        }
        // TODO: check there's current dialog
        _dclDialog.doneDialog(status.value());
        // TODO: what it should return? some (X, Y) point of the dialog
        return new Bool(true);
    }));
    context.setSym('UNLOAD_DIALOG', new Fun('unload_dialog', ['dcl_id'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('unload_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('unload_dialog: too many arguments');
        }
        const dclId = ensureType('unload_dialog:', args[0], [Int]);
        delete _dclFiles[dclId.value()];
        return new Bool(false);
    }));
    context.setSym('ACTION_TILE', new Fun('action_tile', ['key', 'handler'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('action_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('action_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('action_tile: `key`', args[0], [Str]);
        const handler = ensureType('action_tile: `handler`', args[1], [Str]);
        console.log(handler.toUnescapedString());
        try {
            _dclDialog.actionTile(key.value(), handler.value(), context);
            return new Bool(true);
        } catch (e) {
            console.error(e);
            return new Bool(false);
        }
    }));
    context.setSym('GET_TILE', new Fun('get_tile', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('get_tile: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('get_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('get_tile:', args[0], [Str]);
        const str = _dclDialog.getTile(key.value());
        return new Str(str);
    }));
    context.setSym('SET_TILE', new Fun('get_tile', ['key', 'value'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('set_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('set_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('set_tile:', args[0], [Str]);
        const value = ensureType('set_tile:', args[1], [Str]);
        _dclDialog.setTile(key.value(), value.value());
        return value;
    }));
    context.setSym('MODE_TILE', new Fun('mode_tile', ['key', 'mode'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('mode_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('mode_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('mode_tile: `key`', args[0], [Str]);
        const mode = ensureType('mode_tile: `mode`', args[1], [Int]);
        _dclDialog.modeTile(key.value(), mode.value());
        return new Bool(false);
    }));
}
