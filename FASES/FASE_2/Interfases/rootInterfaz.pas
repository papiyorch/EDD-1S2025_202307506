unit rootInterfaz;

interface
    procedure showRootWindow;

    implementation

        uses
            gtk2, glib2, gdk2, login, InterfaceTools, ListaSimple, variables, jsonTools, filesTools, comunidades, listaDeLista, AbolBST, verMensajesComunidad;

        var
            rootWindow: PGtkWidget;
            botonCrearUser, botonReportes, botonCargaMasiva, btnComunidades, btnMensajesComunidad, botonSalir: PGtkWidget;

        procedure cargaMasivaClick(widget: PGtkWidget; data: gpointer); cdecl;
        var
            status: Boolean;
            statusCorreos: Boolean;
        begin
            status := jsonTools.cargaUsuariosJson(json_file_path);
            statusCorreos := jsonTools.cargarCorreosJson(json_correos_path);
            if status then
                begin
                    mostrarMensajeLogin(rootWindow, 'Carga Masiva', 'Usuarios cargados exitosamente.')
                end
            else
                begin
                    mostrarMensajeError(rootWindow, 'Carga Masiva', 'Error al cargar usuarios.');
                end;

            if statusCorreos then
                begin
                    mostrarMensajeLogin(rootWindow, 'Carga Masiva', 'Correos cargados exitosamente.')
                end
            else
                begin
                    mostrarMensajeError(rootWindow, 'Carga Masiva', 'Error al cargar correos.');
                end;
        end;

        procedure generarReportesClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            filesTools.generarReportes('users', 'Root-Reportes', generarDotLS);
            filesTools.generarReportes('comunidades', 'Root-Reportes', listaDeLista.generarDotListaDeLista(listaComunidades));
            filesTools.generarReportes('ArbolBST', 'Root-Reportes', AbolBST.generarDOTBST(raiz));
        end;

        procedure mostrarComunidadesClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            showComunidadesWindow;
        end;

        procedure mostrarMensajesComunidadClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            showMensajesComunidadWindow;
        end;

        procedure cerrarSesion(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            gtk_widget_destroy(rootWindow);
            ventanaLogin;
        end;

        procedure showRootWindow;
            var
                grid: PGtkWidget;

            begin
                gtk_init(@argc, @argv);

                rootWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
                gtk_window_set_title(GTK_WINDOW(rootWindow), 'Panel de Administración');
                gtk_container_set_border_width(GTK_CONTAINER(rootWindow), 10);
                gtk_window_set_default_size(GTK_WINDOW(rootWindow), 800, 600);

                grid := gtk_table_new(6, 1, False);
                gtk_container_add(GTK_CONTAINER(rootWindow), grid);

                botonCrearUser := gtk_button_new_with_label('Reporte de Relaciones');
                botonReportes := gtk_button_new_with_label('Reportes');
                botonCargaMasiva := gtk_button_new_with_label('Carga Masiva');
                btnComunidades := gtk_button_new_with_label('Comunidades');
                btnMensajesComunidad := gtk_button_new_with_label('Mensajes Comunidad');
                botonSalir := gtk_button_new_with_label('Cerrar Sesion');

                g_signal_connect(botonCargaMasiva, 'clicked', G_CALLBACK(@cargaMasivaClick), nil);
                g_signal_connect(botonReportes, 'clicked', G_CALLBACK(@generarReportesClick), nil);
                g_signal_connect(botonSalir, 'clicked', G_CALLBACK(@cerrarSesion), nil);
                g_signal_connect(btnComunidades, 'clicked', G_CALLBACK(@mostrarComunidadesClick), nil);
                g_signal_connect(btnMensajesComunidad, 'clicked', G_CALLBACK(@mostrarMensajesComunidadClick), nil);

                gtk_table_attach_defaults(GTK_TABLE(grid), botonCrearUser, 0, 1, 0, 1);
                gtk_table_attach_defaults(GTK_TABLE(grid), botonReportes, 0, 1, 1, 2);
                gtk_table_attach_defaults(GTK_TABLE(grid), botonCargaMasiva, 0, 1, 2, 3);
                gtk_table_attach_defaults(GTK_TABLE(grid), btnComunidades, 0, 1, 3, 4);
                gtk_table_attach_defaults(GTK_TABLE(grid), btnMensajesComunidad, 0, 1, 4, 5);
                gtk_table_attach_defaults(GTK_TABLE(grid), botonSalir, 0, 1, 5, 6);

                gtk_widget_show_all(rootWindow);
                g_signal_connect(rootWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

                gtk_main;
            end;
end.
