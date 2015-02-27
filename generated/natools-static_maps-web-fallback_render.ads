--  Generated at 2015-02-27 22:21:51 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-fallback_render-maps.sx

package Natools.Static_Maps.Web.Fallback_Render is
   pragma Pure;

   type Command is
     (Unknown,
      Current_Time,
      Parameter,
      Set_MIME_Type);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "now";
   Map_1_Key_1 : aliased constant String := "today";
   Map_1_Key_2 : aliased constant String := "current-time";
   Map_1_Key_3 : aliased constant String := "current-date";
   Map_1_Key_4 : aliased constant String := "parameter";
   Map_1_Key_5 : aliased constant String := "mime-type";
   Map_1_Keys : constant array (0 .. 5) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access);
   Map_1_Elements : constant array (0 .. 5) of Command
     := (Current_Time,
         Current_Time,
         Current_Time,
         Current_Time,
         Parameter,
         Set_MIME_Type);

end Natools.Static_Maps.Web.Fallback_Render;
