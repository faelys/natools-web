--  Generated at 2014-12-16 21:02:50 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-tags-maps.sx

package Natools.Static_Maps.Web.Tags is
   pragma Pure;

   type List_Command is
     (Unknown_List_Command,
      All_Elements,
      Current_Element,
      Greater_Elements,
      Full_Name,
      Last_Name,
      Lesser_Elements,
      Name);

   function To_List_Command (Key : String) return List_Command;

private

   Map_1_Key_0 : aliased constant String := "all-elements";
   Map_1_Key_1 : aliased constant String := "current-element";
   Map_1_Key_2 : aliased constant String := "greater-elements";
   Map_1_Key_3 : aliased constant String := "full-name";
   Map_1_Key_4 : aliased constant String := "last-name";
   Map_1_Key_5 : aliased constant String := "lesser-elements";
   Map_1_Key_6 : aliased constant String := "name";
   Map_1_Keys : constant array (0 .. 6) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access);
   Map_1_Elements : constant array (0 .. 6) of List_Command
     := (All_Elements,
         Current_Element,
         Greater_Elements,
         Full_Name,
         Last_Name,
         Lesser_Elements,
         Name);

end Natools.Static_Maps.Web.Tags;
