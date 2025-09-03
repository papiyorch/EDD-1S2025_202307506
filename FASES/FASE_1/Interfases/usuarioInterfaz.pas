unit usuarioInterfaz;

interface
    procedure showUsuarioWindow;

implementation
    uses
        SysUtils, gtk2, glib2, gdk2, variables, ListaSimple, ListaCircular, addContactInterface, filesTools, contactosInfo, enviarCorreo, ListaDoble;

    var 
        usuarioWindow: PGtkWidget;
        btnBandeja, btnEnviar, btnPapelera, btnProgramar, btnCorreosP, btnAgregarContactos, btnContactos, btnActualizar, btnReportes, btnLogOut: PGtkWidget;


    procedure agregarContacto(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(usuarioWindow);
        showAddContactWindow;
    end;

    procedure generarReportes(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        filesTools.generarReportes('contactos', 'Contactos-Reportes', ListaCircular.generarDotLC);
        filesTools.generarReportes('correos', 'Correos-Reportes', ListaDoble.generarDotLD);
    end;

    procedure informacionContactos(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        contactosInfo.showContactInfoWindow;
    end;

    procedure correo(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        enviarCorreo.showEnviarCorreoWindow;
    end;

    procedure showUsuarioWindow;

    var 
        grid: PGtkWidget;

    begin
        gtk_init(@argc, @argv);

        usuarioWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(usuarioWindow), 'Panel de Usuario');
        gtk_container_set_border_width(GTK_CONTAINER(usuarioWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(usuarioWindow), 800, 600);

        grid := gtk_table_new(5, 1, False);
        gtk_container_add(GTK_CONTAINER(usuarioWindow), grid);

        btnBandeja := gtk_button_new_with_label('Bandeja de Entrada');
        btnEnviar := gtk_button_new_with_label('Enviar Correo');
        btnPapelera := gtk_button_new_with_label('Papelera');
        btnProgramar := gtk_button_new_with_label('Programar Correo');
        btnCorreosP := gtk_button_new_with_label('Correos Programados');
        btnAgregarContactos := gtk_button_new_with_label('Agregar Contactos');
        btnContactos := gtk_button_new_with_label('Ver Contactos');
        btnActualizar := gtk_button_new_with_label('Actualizar Datos');
        btnReportes := gtk_button_new_with_label('Generar Reportes');
        btnLogOut := gtk_button_new_with_label('Cerrar Sesion');

        g_signal_connect(btnAgregarContactos, 'clicked', G_CALLBACK(@agregarContacto), nil);
        g_signal_connect(btnReportes, 'clicked', G_CALLBACK(@generarReportes), nil);
        g_signal_connect(btnContactos, 'clicked', G_CALLBACK(@informacionContactos), nil);
        g_signal_connect(btnEnviar, 'clicked', G_CALLBACK(@correo), nil);

        gtk_table_attach_defaults(GTK_TABLE(grid), btnBandeja, 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnEnviar, 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnPapelera, 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnProgramar, 0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnCorreosP, 0, 1, 4, 5);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnAgregarContactos, 0, 1, 5, 6);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnContactos, 0, 1, 6, 7);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnActualizar, 0, 1, 7, 8);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnReportes, 0, 1, 8, 9);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnLogOut, 0, 1, 9, 10);

        gtk_widget_show_all(usuarioWindow);

        g_signal_connect(usuarioWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_main;
    end;
end.