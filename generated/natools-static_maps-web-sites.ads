--  Generated at 2015-03-21 21:27:24 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-sites-maps.sx

package Natools.Static_Maps.Web.Sites is
   pragma Pure;

   type Command is
     (Error,
      Set_Backends,
      Set_Default_Template,
      Set_File_Prefix,
      Set_File_Suffix,
      Set_Filters,
      Set_Named_Element_File,
      Set_Named_Elements,
      Set_Path_Prefix,
      Set_Path_Suffix,
      Set_Printer,
      Set_Static_Paths,
      Set_Template_File,
      Set_Templates);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "backends";
   Map_1_Key_1 : aliased constant String := "back-ends";
   Map_1_Key_2 : aliased constant String := "default-template";
   Map_1_Key_3 : aliased constant String := "file-prefix";
   Map_1_Key_4 : aliased constant String := "file-suffix";
   Map_1_Key_5 : aliased constant String := "filters";
   Map_1_Key_6 : aliased constant String := "named-element-file";
   Map_1_Key_7 : aliased constant String := "named-elements";
   Map_1_Key_8 : aliased constant String := "path-prefix";
   Map_1_Key_9 : aliased constant String := "path-suffix";
   Map_1_Key_10 : aliased constant String := "printer";
   Map_1_Key_11 : aliased constant String := "printer-parameters";
   Map_1_Key_12 : aliased constant String := "static";
   Map_1_Key_13 : aliased constant String := "static-path";
   Map_1_Key_14 : aliased constant String := "static-paths";
   Map_1_Key_15 : aliased constant String := "template-file";
   Map_1_Key_16 : aliased constant String := "templates";
   Map_1_Keys : constant array (0 .. 16) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access,
         Map_1_Key_9'Access,
         Map_1_Key_10'Access,
         Map_1_Key_11'Access,
         Map_1_Key_12'Access,
         Map_1_Key_13'Access,
         Map_1_Key_14'Access,
         Map_1_Key_15'Access,
         Map_1_Key_16'Access);
   Map_1_Elements : constant array (0 .. 16) of Command
     := (Set_Backends,
         Set_Backends,
         Set_Default_Template,
         Set_File_Prefix,
         Set_File_Suffix,
         Set_Filters,
         Set_Named_Element_File,
         Set_Named_Elements,
         Set_Path_Prefix,
         Set_Path_Suffix,
         Set_Printer,
         Set_Printer,
         Set_Static_Paths,
         Set_Static_Paths,
         Set_Static_Paths,
         Set_Template_File,
         Set_Templates);

end Natools.Static_Maps.Web.Sites;
