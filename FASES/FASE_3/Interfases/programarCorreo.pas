unit programarCorreo;

interface
    procedure showProgramarCorreoWindow;

implementation
    uses
        SysUtils, Classes, gtk2, glib2, gdk2, variables, Cola, ListaSimple, InterfaceTools, ListaCircular;

    var 
        programarWindow: PGtkWidget;
        entryDestinatario, entryAsunto, entryFecha, entryMensaje: PGtkWidget;
        btnProgramar: PGtkWidget;
        contadorProgramados: Integer = 0;

    procedure programarCorreo(widget: PGtkWidget; data: gpointer); cdecl;
    var
        destinatarioP, asunto, mensaje, remitente, estado, idCorreo: String;
        programado: Boolean;
        fechaEnvio: TDateTime;
        contactoDestino: PNodo;
        fechaTexto: String;

    begin
        destinatarioP := gtk_entry_get_text(GTK_ENTRY(entryDestinatario));
        asunto := gtk_entry_get_text(GTK_ENTRY(entryAsunto));
        mensaje := gtk_entry_get_text(GTK_ENTRY(entryMensaje));
        fechaTexto := gtk_entry_get_text(GTK_ENTRY(entryFecha));
        if not TryStrToDateTime(fechaTexto, fechaEnvio) then
        begin
            fechaTexto := StringReplace(fechaTexto, '-', '/', [rfReplaceAll]);
            if not TryStrToDateTime(fechaTexto, fechaEnvio) then
            begin
                mostrarMensajeError(programarWindow, 'Error', 'La fecha de envío no es válida. Usa: DD-MM-YYYY HH:MM:SS');
                Exit;
            end;
        end;

        remitente := usuarioActual^.email;
        estado := 'Programado';
        programado := True;

        Inc(contadorProgramados);
        idCorreo := IntToStr(contadorProgramados);

        if not existeContacto(usuarioActual^.contactos, destinatarioP) then
        begin
            mostrarMensajeError(programarWindow, 'Error', 'El destinatario no existe en tus contactos.');
            Exit;
        end;

        contactoDestino := buscarUsuarioPorEmail(listaUsuarios, destinatarioP);

        if contactoDestino = nil then
        begin
            mostrarMensajeError(programarWindow, 'Error', 'El destinatario no está registrado en el sistema.');
            Exit;
        end;
        //Se inserta el correo programado en la cola del usuario actual
        insertarCola(usuarioActual^.correosProgramados, idCorreo, remitente, estado, programado, asunto, fechaEnvio, mensaje, destinatarioP);
        mostrarMensajeLogin(programarWindow, 'Éxito', 'Correo programado correctamente.');
        imprimirCola(usuarioActual^.correosProgramados);
    end;

    procedure showProgramarCorreoWindow;
    var
        grid: PGtkWidget;
    begin

        gtk_init(@argc, @argv);

        programarWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(programarWindow), 'Programar Correo');
        gtk_container_set_border_width(GTK_CONTAINER(programarWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(programarWindow), 400, 300);

        grid := gtk_table_new(5, 2, False);
        gtk_container_add(GTK_CONTAINER(programarWindow), grid);

        entryDestinatario := gtk_entry_new();
        entryAsunto := gtk_entry_new();
        entryFecha := gtk_entry_new();
        entryMensaje := gtk_entry_new();

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Destinatario :'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryDestinatario, 1, 2, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Asunto :'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryAsunto, 1, 2, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Fecha de Envío:'), 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryFecha, 1, 2, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Mensaje :'), 0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryMensaje, 1, 2, 3, 4);

        btnProgramar := gtk_button_new_with_label('Programar');
        g_signal_connect(btnProgramar, 'clicked', G_CALLBACK(@programarCorreo), nil);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnProgramar, 0, 2, 4, 5);

        gtk_widget_show_all(programarWindow);
        g_signal_connect(programarWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
        gtk_main;
    end;
end.

