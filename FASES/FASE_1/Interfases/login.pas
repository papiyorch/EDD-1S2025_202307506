unit login;

interface
    procedure ventanaLogin;

implementation

    uses
        gtk2, glib2, gdk2,
        rootInterfaz, InterfaceTools,
        ListaSimple, usuarioInterfaz, variables;

    var
        entryMail, entryPassword: PGtkWidget;
        loginWWindow: PGtkWidget;

    //Evento del botón de login
    procedure OnLoginButtonClick(widget: PGtkWidget; data: gpointer); cdecl;
    var
        userText, passwordText: PChar;
        isValid: Boolean;
        userData: TDatos;
    begin
        userText := PChar(gtk_entry_get_text(GTK_ENTRY(entryMail)));
        passwordText := PChar(gtk_entry_get_text(GTK_ENTRY(entryPassword)));

        isValid := validarCredenciales(userText, passwordText);
        if isValid then
        begin
            userData := buscarPorEmail(userText);

            nombreUsuarioActual := userData.nombre;
            emailUsuarioActual := userData.email;
            usuarioActual := obtenerNodoPorEmail(userText);

            WriteLn(nombreUsuarioActual);

            gtk_widget_destroy(loginWWindow);
            showUsuarioWindow();
        end
        else if (userText = rootUserEmail) and (passwordText = rootUserPassword) then
        begin
            IDUsuarioActual := 0;
            nombreUsuarioActual := 'Root';
            emailUsuarioActual := rootUserEmail;
            usuarioActual := nil;

            gtk_widget_destroy(loginWWindow);
            showRootWindow();
        end
        else
        begin
            mostrarMensajeError(loginWWindow, 'Error de LLogin', 'Credenciales inválidas');
        end;
    end;

    //Ventana de Inicio de Sesion
    procedure ventanaLogin;
    var
        grid: PGtkWidget;
        labelUsuario, labelPassword: PGtkWidget;
        botonLLogin, botonCrearUser: PGtkWidget;
    begin
        gtk_init(@argc, @argv);

        loginWWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(loginWWindow), 'Iniciar Sesión');
        gtk_container_set_border_width(GTK_CONTAINER(loginWWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(loginWWindow), 300, 200);

        grid := gtk_table_new(4,2, False);
        gtk_container_add(GTK_CONTAINER(loginWWindow), grid);

        labelUsuario := gtk_label_new('Email:');
        labelPassword := gtk_label_new('Contraseña:');

        entryMail := gtk_entry_new;
        entryPassword := gtk_entry_new;
        gtk_entry_set_visibility(GTK_ENTRY(entryPassword), False);

        botonLLogin := gtk_button_new_with_label('Iniciar Sesión');
        g_signal_connect(botonLLogin, 'clicked', G_CALLBACK(@OnLoginButtonClick), nil);

        botonCrearUser := gtk_button_new_with_label('Crear Usuario');
        //g_signal_connect(botonCrearUser, 'clicked', G_CALLBACK(@ONLCrearUserButtonClick), nil);

        gtk_table_attach_defaults(GTK_TABLE(grid), labelUsuario, 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryMail, 1, 2, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), labelPassword, 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryPassword, 1, 2, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), botonLLogin, 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), botonCrearUser, 1, 2, 2, 3);

        gtk_widget_show_all(loginWWindow);
        g_signal_connect(loginWWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_main;
    end;
end.
