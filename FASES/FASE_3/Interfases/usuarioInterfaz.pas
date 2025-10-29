unit usuarioInterfaz;

interface
    procedure showUsuarioWindow;

implementation
    uses
        SysUtils, gtk2, glib2, gdk2, variables, ListaSimple, ListaCircular, addContactInterface, filesTools, contactosInfo, enviarCorreo, ListaDoble, Pila, bandejaEntrada, 
        papelera, login, programarCorreo, Cola, correosPInterfaz, perfilUsuario, ArbolB, verFavoritos, verBorradores, ArbolAVL, ArbolMerkle, publicarMensaje, verPrivados;

    var 
        usuarioWindow: PGtkWidget;
        btnBandeja, btnEnviar, btnPapelera, btnProgramar, btnCorreosP, btnAgregarContactos: PGtkWidget;
        btnContactos, btnBorradores, btnActualizar, btnReportes, btnLogOut, btnFavoritos, btnPublicar, btnVerPrivados: PGtkWidget;

    procedure papelera(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showPapeleraWindow;
    end;

    procedure agregarContacto(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(usuarioWindow);
        showAddContactWindow;
    end;

    procedure generarReportes(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        filesTools.generarReportes('contactos', 'Usuarios-Reportes', ListaCircular.generarDotLC(usuarioActual^.contactos));
        filesTools.generarReportes('correos', 'Usuarios-Reportes', ListaDoble.generarDotLD(usuarioActual^.correos));
        filesTools.generarReportes('papelera', 'Usuarios-Reportes', Pila.generarDotPila(usuarioActual^.papelera));
        filesTools.generarReportes('correos_programados', 'Usuarios-Reportes', Cola.generarDotCola(usuarioActual^.correosProgramados));
        filesTools.generarReportes('favoritos', 'Usuarios-Reportes', ArbolB.generarDotFavoritos(usuarioActual^.favoritos));
        filesTools.generarReportes('Borradores', 'Usuarios-Reportes', ArbolAVL.GenerarDOTInOrden(usuarioActual^.borradores));
        filesTools.generarReportes('Privados', 'Usuarios-Reportes', usuarioActual^.privados.GenerateDot);
    end;

    procedure bandejaEntrada(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showBandejaEntradaWindow;
    end;

    procedure informacionContactos(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        contactosInfo.showContactInfoWindow;
    end;

    procedure correo(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        enviarCorreo.showEnviarCorreoWindow;
    end;

    procedure cerrarSesionUsuario(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        RegistrarSalida(usuarioActual^.usuario);
        gtk_widget_destroy(usuarioWindow);
        ventanaLogin;
    end;

    procedure programarCorreo(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showProgramarCorreoWindow;
    end;

    procedure verCorreosProgramados(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showProgramadosWindow;
    end;

    procedure verFavoritos(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showVerFavoritosWindow;
    end;

    procedure verDatosUsuario(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showPerfilUsuarioWindow;
    end;

    procedure verBorradores(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showVerBorradoresWindow;
    end;

    procedure publicarMensajeEnComunidad(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showPublicarMensajeWindow;
    end;

    procedure verPrivados(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showVerPrivadosWindow;
    end;

    procedure showUsuarioWindow;

    var 
        grid: PGtkWidget;

    begin
        gtk_init(@argc, @argv);

        usuarioWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(usuarioWindow), 'Panel de Usuario');
        gtk_container_set_border_width(GTK_CONTAINER(usuarioWindow), 16);
        gtk_window_set_default_size(GTK_WINDOW(usuarioWindow), 800, 600);

        grid := gtk_table_new(5, 1, False);
        gtk_container_add(GTK_CONTAINER(usuarioWindow), grid);

        btnBandeja := gtk_button_new_with_label('Bandeja de Entrada');
        btnEnviar := gtk_button_new_with_label('Enviar Correo');
        btnPapelera := gtk_button_new_with_label('Papelera');
        btnProgramar := gtk_button_new_with_label('Programar Correo');
        btnCorreosP := gtk_button_new_with_label('Correos Programados');
        btnFavoritos := gtk_button_new_with_label('Ver Favoritos');
        btnAgregarContactos := gtk_button_new_with_label('Agregar/Eliminar Contactos');
        btnContactos := gtk_button_new_with_label('Ver Contactos');
        btnActualizar := gtk_button_new_with_label('Actualizar Datos');
        btnReportes := gtk_button_new_with_label('Generar Reportes');
        btnLogOut := gtk_button_new_with_label('Cerrar Sesion');
        btnBorradores := gtk_button_new_with_label('Ver Borradores');
        btnPublicar := gtk_button_new_with_label('Publicar');
        btnVerPrivados := gtk_button_new_with_label('Privados');

        g_signal_connect(btnAgregarContactos, 'clicked', G_CALLBACK(@agregarContacto), nil);
        g_signal_connect(btnReportes, 'clicked', G_CALLBACK(@generarReportes), nil);
        g_signal_connect(btnContactos, 'clicked', G_CALLBACK(@informacionContactos), nil);
        g_signal_connect(btnEnviar, 'clicked', G_CALLBACK(@correo), nil);
        g_signal_connect(btnBandeja, 'clicked', G_CALLBACK(@bandejaEntrada), nil);
        g_signal_connect(btnLogOut, 'clicked', G_CALLBACK(@cerrarSesionUsuario), nil);
        g_signal_connect(btnPapelera, 'clicked', G_CALLBACK(@papelera), nil);
        g_signal_connect(btnProgramar, 'clicked', G_CALLBACK(@programarCorreo), nil);
        g_signal_connect(btnCorreosP, 'clicked', G_CALLBACK(@verCorreosProgramados), nil);
        g_signal_connect(btnActualizar, 'clicked', G_CALLBACK(@verDatosUsuario), nil);
        g_signal_connect(btnFavoritos, 'clicked', G_CALLBACK(@verFavoritos), nil);
        g_signal_connect(btnBorradores, 'clicked', G_CALLBACK(@verBorradores), nil);
        g_signal_connect(btnPublicar, 'clicked', G_CALLBACK(@publicarMensajeEnComunidad), nil);
        g_signal_connect(btnVerPrivados, 'clicked', G_CALLBACK(@verPrivados), nil);

        gtk_table_attach_defaults(GTK_TABLE(grid), btnBandeja, 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnEnviar, 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnPapelera, 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnProgramar, 0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnCorreosP, 0, 1, 4, 5);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnFavoritos, 0, 1, 5, 6);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnAgregarContactos, 0, 1, 6, 7);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnContactos, 0, 1, 7, 8);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnActualizar, 0, 1, 8, 9);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnReportes, 0, 1, 9, 10);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnLogOut, 0, 1, 10, 11);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnBorradores, 0, 1, 11, 12);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnPublicar, 0, 1, 12, 13);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnVerPrivados, 0, 1, 13, 14);

        gtk_widget_show_all(usuarioWindow);

        g_signal_connect(usuarioWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_main;
    end;
end.