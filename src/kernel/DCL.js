const {Bool, Int, Sym, Str, List, Fun, ensureType} = require('../VeLispTypes.js');
const VeDclDialogsLoader = require('../VeDclDialogsLoader.js');
const {VeDclGtkDialog} = require('../VeDclGtkDialog.js');

const gi = require('node-gtk');
const Gtk = gi.require('Gtk', '3.0');

gi.startLoop();
Gtk.init();

let _dialogs = null;
let _dialog = null;

exports.initContext = function (context) {
    context.setSym('LOAD_DIALOG', new Fun('load_dialog', ['dcl_file'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('load_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('load_dialog: too many arguments');
        }
        const dclFile = ensureType('load_dialog:', args[0], [Str]);
        // TODO: store to context.setDialogs(...)?
        // TODO: dialogs should be a {"dlg1": Dialog(), "dlg2", Dialog}
        const dialogs = VeDclDialogsLoader.load(dclFile.value());
        console.log(dialogs);
        _dialogs = dialogs;
        return new Sym(dclFile);
    }));
    context.setSym('NEW_DIALOG', new Fun('new_dialog', ['dlg_id', 'dcl_id'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('new_dialog: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('new_dialog: too many arguments');
        }
        // TODO: lookup dcl_id first
        // TODO: lookup dlg_id then
        // TODO: put on top of some stack that (start_dialog) will run
        _dialog = _dialogs[0];
        return new Bool(true);
    }));
    context.setSym('START_DIALOG', new Fun('start_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('start_dialog: too many arguments');
        }

        const gtkDialog = new VeDclGtkDialog(_dialog);
        const gtkXml = gtkDialog.toString();

        const builder = new Gtk.Builder();
        builder.addFromString(gtkXml, gtkXml.length);
        const dlg = builder.getObject(_dialog.id)
        const ret = dlg.run();
        console.log(ret);
        return new Int(ret);

        /*
        const win = new Gtk.Window();
        win.on('destroy', () => Gtk.mainQuit());
        win.on('delete-event', () => false);

        win.setDefaultSize(200, 80);
        win.add(new Gtk.Label({ label: 'Hello Gtk+' }));

        win.showAll();
        Gtk.main();
        */
    }));
    context.setSym('UNLOAD_DIALOG', new Fun('unload_dialog', ['dcl_id'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('unload_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('unload_dialog: too many arguments');
        }
        const dclId = ensureType('unload_dialog:', args[0], [Sym]);
        // TODO: unregister dialogs
        // TODO: return nil if there's no such dlg_id
        return new Bool(true);
    }));
}
