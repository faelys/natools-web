--  Generated at 2015-02-03 21:39:53 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-error_pages-maps.sx

package Natools.Static_Maps.Web.Error_Pages is
   pragma Pure;

   type Command is
     (Unknown_Command,
      Location,
      Message,
      Path,
      Status_Code);

   function To_Command (Key : String) return Command;
   function To_Message (Key : String) return String;

private

   Map_1_Key_0 : aliased constant String := "location";
   Map_1_Key_1 : aliased constant String := "message";
   Map_1_Key_2 : aliased constant String := "path";
   Map_1_Key_3 : aliased constant String := "code";
   Map_1_Key_4 : aliased constant String := "status";
   Map_1_Key_5 : aliased constant String := "status-code";
   Map_1_Keys : constant array (0 .. 5) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access);
   Map_1_Elements : constant array (0 .. 5) of Command
     := (Location,
         Message,
         Path,
         Status_Code,
         Status_Code,
         Status_Code);

   Map_2_Key_0 : aliased constant String := "301";
   Map_2_Key_1 : aliased constant String := "303";
   Map_2_Key_2 : aliased constant String := "404";
   Map_2_Key_3 : aliased constant String := "405";
   Map_2_Key_4 : aliased constant String := "410";
   Map_2_Keys : constant array (0 .. 4) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access);
   subtype Map_2_Hash is Natural range 0 .. 4;
   function Map_2_Elements (Hash : Map_2_Hash)
     return String;

end Natools.Static_Maps.Web.Error_Pages;
