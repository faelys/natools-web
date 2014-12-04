--  Generated at 2014-12-04 18:01:45 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-list_templates-maps.sx

package Natools.Static_Maps.Web.List_Templates is
   pragma Pure;

   type Command is
     (Unknown_Command,
      Backward,
      Ellipsis_Prefix,
      Ellipsis_Suffix,
      Forward,
      Length_Limit,
      Template);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "backward";
   Map_1_Key_1 : aliased constant String := "backwards";
   Map_1_Key_2 : aliased constant String := "reverse";
   Map_1_Key_3 : aliased constant String := "ellipsis-prefix";
   Map_1_Key_4 : aliased constant String := "ellipsis-suffix";
   Map_1_Key_5 : aliased constant String := "ellipsis";
   Map_1_Key_6 : aliased constant String := "forward";
   Map_1_Key_7 : aliased constant String := "forwards";
   Map_1_Key_8 : aliased constant String := "length-limit";
   Map_1_Key_9 : aliased constant String := "limit";
   Map_1_Key_10 : aliased constant String := "max";
   Map_1_Key_11 : aliased constant String := "maximum";
   Map_1_Key_12 : aliased constant String := "template";
   Map_1_Key_13 : aliased constant String := "render";
   Map_1_Keys : constant array (0 .. 13) of access constant String
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
         Map_1_Key_13'Access);
   Map_1_Elements : constant array (0 .. 13) of Command
     := (Backward,
         Backward,
         Backward,
         Ellipsis_Prefix,
         Ellipsis_Suffix,
         Ellipsis_Suffix,
         Forward,
         Forward,
         Length_Limit,
         Length_Limit,
         Length_Limit,
         Length_Limit,
         Template,
         Template);

end Natools.Static_Maps.Web.List_Templates;
