--  Generated at 2014-10-16 21:38:06 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-sites-maps.sx

package Natools.Static_Maps.Web.Sites is
   pragma Pure;

   type Command is
     (Error,
      Set_Default_Template,
      Set_File_Prefix,
      Set_File_Suffix,
      Set_Path_Prefix,
      Set_Path_Suffix,
      Set_Template_File,
      Set_Templates,
      Site_Map);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "default-template";
   Map_1_Key_1 : aliased constant String := "file-prefix";
   Map_1_Key_2 : aliased constant String := "file-suffix";
   Map_1_Key_3 : aliased constant String := "path-prefix";
   Map_1_Key_4 : aliased constant String := "path-suffix";
   Map_1_Key_5 : aliased constant String := "template-file";
   Map_1_Key_6 : aliased constant String := "templates";
   Map_1_Key_7 : aliased constant String := "site-map";
   Map_1_Key_8 : aliased constant String := "pages";
   Map_1_Keys : constant array (0 .. 8) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access);
   Map_1_Elements : constant array (0 .. 8) of Command
     := (Set_Default_Template,
         Set_File_Prefix,
         Set_File_Suffix,
         Set_Path_Prefix,
         Set_Path_Suffix,
         Set_Template_File,
         Set_Templates,
         Site_Map,
         Site_Map);

end Natools.Static_Maps.Web.Sites;
