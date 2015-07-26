--  Generated at 2015-07-26 11:31:34 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-list_templates-maps.sx

package Natools.Static_Maps.Web.List_Templates is
   pragma Pure;

   type Command is
     (Unknown_Command,
      Backward,
      Ellipsis_Prefix,
      Ellipsis_Suffix,
      Forward,
      If_Empty,
      Length_Limit,
      Show_Beginning,
      Show_Ending,
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
   Map_1_Key_8 : aliased constant String := "if-empty";
   Map_1_Key_9 : aliased constant String := "length-limit";
   Map_1_Key_10 : aliased constant String := "limit";
   Map_1_Key_11 : aliased constant String := "max";
   Map_1_Key_12 : aliased constant String := "maximum";
   Map_1_Key_13 : aliased constant String := "show-beginning";
   Map_1_Key_14 : aliased constant String := "beginning";
   Map_1_Key_15 : aliased constant String := "show-firsts";
   Map_1_Key_16 : aliased constant String := "show-ending";
   Map_1_Key_17 : aliased constant String := "ending";
   Map_1_Key_18 : aliased constant String := "show-lasts";
   Map_1_Key_19 : aliased constant String := "template";
   Map_1_Key_20 : aliased constant String := "render";
   Map_1_Keys : constant array (0 .. 20) of access constant String
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
         Map_1_Key_16'Access,
         Map_1_Key_17'Access,
         Map_1_Key_18'Access,
         Map_1_Key_19'Access,
         Map_1_Key_20'Access);
   Map_1_Elements : constant array (0 .. 20) of Command
     := (Backward,
         Backward,
         Backward,
         Ellipsis_Prefix,
         Ellipsis_Suffix,
         Ellipsis_Suffix,
         Forward,
         Forward,
         If_Empty,
         Length_Limit,
         Length_Limit,
         Length_Limit,
         Length_Limit,
         Show_Beginning,
         Show_Beginning,
         Show_Beginning,
         Show_Ending,
         Show_Ending,
         Show_Ending,
         Template,
         Template);

end Natools.Static_Maps.Web.List_Templates;
