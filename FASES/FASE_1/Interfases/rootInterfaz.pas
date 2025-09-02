unit rootInterfaz;

interface
    procedure showRootWindow;

    implementation

        uses
            gtk2, glib2, gdk2,
            login, InterfaceTools,
            ListaSimple, variables, jsonTools, filesTools;

        var
            rootWindow: PGtkWidget;
            botonCrearUser, botonReportes, botonCargaMasiva, botonSalir: PGtkWidget;

        procedure cargaMasivaClick(widget: PGtkWidget; data: gpointer); cdecl;
        var
            status: Boolean;
        begin
            status := jsonTools.cargaUsuariosJson(json_file_path);
            if status then
                begin
                    imprimir; // Muestra en consola los usuarios cargados
                    mostrarMensajeLogin(rootWindow, 'Carga Masiva', 'Usuarios cargados exitosamente.')
                end
            else
                begin
                    mostrarMensajeError(rootWindow, 'Carga Masiva', 'Error al cargar usuarios.');
                end;
        end;

        procedure generarReportesClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            filesTools.generarReportes('users', 'Root-Reportes', generarDotLS);
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

                grid := gtk_table_new(5, 1, False);
                gtk_container_add(GTK_CONTAINER(rootWindow), grid);

                botonCrearUser := gtk_button_new_with_label('Crear Usuario');
                botonReportes := gtk_button_new_with_label('Generar Reportes');
                botonCargaMasiva := gtk_button_new_with_label('Carga Masiva');
                botonSalir := gtk_button_new_with_label('Cerrar Sesion');

                g_signal_connect(botonCargaMasiva, 'clicked', G_CALLBACK(@cargaMasivaClick), nil);
                g_signal_connect(botonReportes, 'clicked', G_CALLBACK(@generarReportesClick), nil);

                gtk_table_attach_defaults(GTK_TABLE(grid), botonCrearUser, 0, 1, 0, 1);
                gtk_table_attach_defaults(GTK_TABLE(grid), botonReportes, 0, 1, 1, 2);
                gtk_table_attach_defaults(GTK_TABLE(grid), botonCargaMasiva, 0, 1, 2, 3);
                gtk_table_attach_defaults(GTK_TABLE(grid), botonSalir, 0, 1, 3, 4);

                gtk_widget_show_all(rootWindow);
                g_signal_connect(rootWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

                gtk_main;
            end;
end.
