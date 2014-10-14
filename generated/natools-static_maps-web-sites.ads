--  Generated at 2014-10-14 19:58:04 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-sites-maps.sx

package Natools.Static_Maps.Web.Sites is
   pragma Pure;

   type Command is
     (Error,
      Set_File_Prefix,
      Set_File_Suffix,
      Set_Path_Prefix,
      Set_Path_Suffix,
      Site_Map);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "file-prefix";
   Map_1_Key_1 : aliased constant String := "file-suffix";
   Map_1_Key_2 : aliased constant String := "path-prefix";
   Map_1_Key_3 : aliased constant String := "path-suffix";
   Map_1_Key_4 : aliased constant String := "site-map";
   Map_1_Key_5 : aliased constant String := "pages";
   Map_1_Keys : constant array (0 .. 5) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access);
   Map_1_Elements : constant array (0 .. 5) of Command
     := (Set_File_Prefix,
         Set_File_Suffix,
         Set_Path_Prefix,
         Set_Path_Suffix,
         Site_Map,
         Site_Map);

end Natools.Static_Maps.Web.Sites;
