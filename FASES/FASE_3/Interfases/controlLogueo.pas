unit controlLogueo;

{$MODE DELPHI}

interface

    procedure showControlLogueoWindow;

implementation

    uses
        SysUtils, gtk2, glib2, gdk2, variables, ListaSimple, InterfaceTools;

    var
        controlLogueoWindow: PGtkWidget;
        treeView : PGtkWidget;
        listStore : PGtkListStore;
        btnExportarJson : PGtkWidget;

    procedure cargarDatosLogueo;
    var
        temp: PLogueo;
        iter: TGtkTreeIter;
        horaEntradaStr, horaSalidaStr: string;
    begin
        gtk_list_store_clear(listStore);
        temp := cabezaLogueo;
        while temp <> nil do
        begin
            horaEntradaStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', temp^.horaEntrada);
            if temp^.horaSalida <> 0 then
                horaSalidaStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', temp^.horaSalida)
            else
                horaSalidaStr := '---';

            gtk_list_store_append(listStore, @iter);
            gtk_list_store_set(listStore, @iter,
                0, PChar(temp^.usuario),
                1, PChar(horaEntradaStr),
                2, PChar(horaSalidaStr),
                -1);
            temp := temp^.siguiente;
        end;
    end;

    procedure exportarLogueoJson(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        generarJsonLogueo('control_logueo.json');
        mostrarMensajeLogin(controlLogueoWindow, 'Exportar Logueo', 'Datos de logueo exportados a control_logueo.json');
    end;

    procedure showControlLogueoWindow;
    var
        grid : PGtkWidget;
        iter : TGtkTreeIter;
        colUsuario, colEntrada, colSalida : PGtkTreeViewColumn;
    begin
        gtk_init(@argc, @argv);
        controlLogueoWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(controlLogueoWindow), 'Control de Logueo');
        gtk_window_set_default_size(GTK_WINDOW(controlLogueoWindow), 500, 300);
        gtk_container_set_border_width(GTK_CONTAINER(controlLogueoWindow), 10);

        grid := gtk_table_new(2, 1, FALSE);
        gtk_container_add(GTK_CONTAINER(controlLogueoWindow), grid);

        listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));

        colUsuario := gtk_tree_view_column_new_with_attributes('Usuario', gtk_cell_renderer_text_new(), 'text', 0, nil);
        colEntrada := gtk_tree_view_column_new_with_attributes('Hora de Entrada', gtk_cell_renderer_text_new(), 'text', 1, nil);
        colSalida := gtk_tree_view_column_new_with_attributes('Hora de Salida', gtk_cell_renderer_text_new(), 'text', 2, nil);

        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colUsuario);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colEntrada);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colSalida);

        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

        btnExportarJson := gtk_button_new_with_label('Exportar a JSON');
        gtk_table_attach(GTK_TABLE(grid), btnExportarJson, 0, 1, 1, 2, GTK_SHRINK, GTK_SHRINK, 5, 5);
        g_signal_connect(btnExportarJson, 'clicked', G_CALLBACK(@exportarLogueoJson), nil);

        cargarDatosLogueo;
        gtk_widget_show_all(controlLogueoWindow);
        g_signal_connect(controlLogueoWindow, 'destroy', G_CALLBACK(@gtk_widget_destroyed), nil);
        gtk_main;

    end;
end.
