const {Bool, Int, Sym, Str, List, Fun, ensureType} = require('../VeLispTypes.js');
const Evaluator = require('../VeLispEvaluator.js');
const VeDclDialogsLoader = require('../VeDclDialogsLoader.js');

const gi = require('node-gtk');
const Gtk = gi.require('Gtk', '3.0');

gi.startLoop();
Gtk.init();

const _dclFiles = {};
let _gtkBuilder = null;
let _gtkDialog = null;

exports.initContext = function (context) {
    context.setSym('LOAD_DIALOG', new Fun('load_dialog', ['dcl_file'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('load_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('load_dialog: too many arguments');
        }
        const dclFile = ensureType('load_dialog:', args[0], [Str]);
        const jsDialogs = VeDclDialogsLoader.load(dclFile.value());
        console.log(jsDialogs);
        const dclMap = {};
        for (const jsDialog of jsDialogs) {
            dclMap[jsDialog.id] = jsDialog;
        }
        const dclId = new Sym(dclFile.value());
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
        debugger;
        const dclId = ensureType('new_dialog: `dcl_id`', args[1], [Sym]);
        const dclFile = _dclFiles[dclId.value()];
        if (dclFile) {
            const dlgId = ensureType('new_dialog: `dlg_id`', args[0], [Str]);
            const jsDialog = dclFile[dlgId.value()];
            if (jsDialog) {
                const gtkDialogXml = jsDialog.toGtkXml();
                const gtkXml = `
<?xml version="1.0" encoding="UTF-8"?>
<interface>
${gtkDialogXml}
</interface>
`;
                _gtkBuilder = new Gtk.Builder();
                _gtkBuilder.addFromString(gtkXml, gtkXml.length);
                try {
                    _gtkDialog = _gtkBuilder.getObject(dlgId.value());
                    return new Bool(true);
                } catch {
                    // Should never happen since dialog ID is mandatory
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
        // TODO: ensure _gtkDialog
        const ret = _gtkDialog.run();
        console.log(ret);
        return new Int(ret);
    }));
    context.setSym('DONE_DIALOG', new Fun('done_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('done_dialog: too many arguments');
        }
        // TODO: check there's current
        // TODO: what it should return?
        _gtkDialog.destroy();
        return new Bool(true);
    }));
    context.setSym('UNLOAD_DIALOG', new Fun('unload_dialog', ['dcl_id'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('unload_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('unload_dialog: too many arguments');
        }
        const dclId = ensureType('unload_dialog:', args[0], [Sym]);
        const dclFile = _dclFiles[dclId.value()];
        if (dclFile) {
            delete _dclFiles[dclId.value()];
            return new Bool(true);
        }
        return new Bool(false);
    }));
    context.setSym('ACTION_TILE', new Fun('action_tile', ['tile_id', 'handler'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('action_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('action_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const tileId = ensureType('action_tile: `tile_id`', args[0], [Str]);
        const handler = ensureType('action_tile: `handler`', args[1], [Str]);
        try {
            const button = _gtkBuilder.getObject(tileId.value());
            button.on('clicked', () => {
                Evaluator.evaluate(handler.value(), context);
            });
            return new Bool(true);
        } catch {
            return new Bool(false);
        }
    }));
}
