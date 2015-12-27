--  Generated at 2015-12-27 18:42:05 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-fallback_render-maps.sx

package Natools.Static_Maps.Web.Fallback_Render is
   pragma Pure;

   type Command is
     (Unknown,
      Current_Time,
      Element,
      Element_Or_Template,
      Filter,
      If_Has_Element,
      If_Has_Not_Element,
      If_Parameter_Is,
      Load_Date,
      Parameter,
      Set_MIME_Type,
      Tags,
      Template);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "now";
   Map_1_Key_1 : aliased constant String := "today";
   Map_1_Key_2 : aliased constant String := "current-time";
   Map_1_Key_3 : aliased constant String := "current-date";
   Map_1_Key_4 : aliased constant String := "element";
   Map_1_Key_5 : aliased constant String := "render";
   Map_1_Key_6 : aliased constant String := "element-or-template";
   Map_1_Key_7 : aliased constant String := "filter";
   Map_1_Key_8 : aliased constant String := "if-has-element";
   Map_1_Key_9 : aliased constant String := "if-has-not-element";
   Map_1_Key_10 : aliased constant String := "if-parameter-is";
   Map_1_Key_11 : aliased constant String := "load-date";
   Map_1_Key_12 : aliased constant String := "parameter";
   Map_1_Key_13 : aliased constant String := "mime-type";
   Map_1_Key_14 : aliased constant String := "pagelist";
   Map_1_Key_15 : aliased constant String := "page-list";
   Map_1_Key_16 : aliased constant String := "tag";
   Map_1_Key_17 : aliased constant String := "tags";
   Map_1_Key_18 : aliased constant String := "template";
   Map_1_Keys : constant array (0 .. 18) of access constant String
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
         Map_1_Key_18'Access);
   Map_1_Elements : constant array (0 .. 18) of Command
     := (Current_Time,
         Current_Time,
         Current_Time,
         Current_Time,
         Element,
         Element_Or_Template,
         Element_Or_Template,
         Filter,
         If_Has_Element,
         If_Has_Not_Element,
         If_Parameter_Is,
         Load_Date,
         Parameter,
         Set_MIME_Type,
         Tags,
         Tags,
         Tags,
         Tags,
         Template);

end Natools.Static_Maps.Web.Fallback_Render;
