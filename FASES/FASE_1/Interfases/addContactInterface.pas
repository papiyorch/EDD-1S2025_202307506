unit addContactInterface;

interface
    procedure showAddContactWindow;

implementation
    uses
        SysUtils, usuarioInterfaz, gtk2, glib2, gdk2, variables, ListaSimple, ListaCircular, InterfaceTools;

    var
        addContactWindow: PGtkWidget;
        entryMail: PGtkWidget;

    procedure guardarContacto(widget: PGtkWidget; data: gpointer); cdecl;
    var
        emailBuscado: String;
        usuario: TDatos;

    begin
        emailBuscado := Trim(gtk_entry_get_text(GTK_ENTRY(entryMail)));
        usuario := listaSimple.buscarPorEmail(emailBuscado);
        if usuario.email <> '' then
        begin
            InsertarCircular(usuario.id, usuario.nombre, usuario.email, usuario.usuario, usuario.telefono);
            mostrarMensajeLogin(addContactWindow, 'Contacto guardado', 'El contacto ha sido guardado exitosamente.');
        end
        else
        begin
            mostrarMensajeError(addContactWindow, 'Error', 'Usuario no encontrado.');
        end;
        gtk_widget_destroy(addContactWindow);
        showUsuarioWindow
    end;

    procedure showAddContactWindow;

    var
        grid: PGtkWidget;
        labelCorreo, btnAgregar: PGtkWidget;

    begin
        gtk_init(@argc, @argv);

        addContactWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(addContactWindow), 'Agregar Contacto');
        gtk_container_set_border_width(GTK_CONTAINER(addContactWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(addContactWindow), 400, 300);

        grid := gtk_table_new(5, 2, False);
        gtk_container_add(GTK_CONTAINER(addContactWindow), grid);

        entryMail := gtk_entry_new();

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Email:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryMail, 1, 2, 0, 1);

        btnAgregar := gtk_button_new_with_label('Agregar');
        g_signal_connect(btnAgregar, 'clicked', G_CALLBACK(@guardarContacto), nil);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnAgregar, 0, 2, 1, 2);

        gtk_widget_show_all(addContactWindow);

        g_signal_connect(addContactWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_main;
    end;
end.