pub usingnamespace @cImport({
    @cInclude("fenv.h");

    @cDefine("CIMGUI_USE_GLFW", {});
    @cDefine("CIMGUI_USE_OPENGL3", {});
    @cDefine("CIMGUI_NO_EXPORT", {});
    @cDefine("CIMGUI_DEFINE_ENUMS_AND_STRUCTS", {});

    @cInclude("cimgui.h");
    @cInclude("generator/output/cimgui_impl.h");
});
