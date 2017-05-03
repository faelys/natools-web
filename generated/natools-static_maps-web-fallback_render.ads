--  Generated at 2017-05-03 18:58:11 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-fallback_render-maps.sx

package Natools.Static_Maps.Web.Fallback_Render is
   pragma Pure;

   type Command is
     (Unknown,
      Current_Time,
      Cookies,
      Comment_Cookie_Filter,
      Comment_Cookie_Link,
      Comment_Cookie_Mail,
      Comment_Cookie_Name,
      Element,
      Element_Or_Template,
      Filter,
      If_Has_Element,
      If_Has_Not_Element,
      If_Parameter_Is,
      Load_Date,
      Optional_Tags,
      Parameter,
      Set_MIME_Type,
      Tags,
      Template,
      User);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "now";
   Map_1_Key_1 : aliased constant String := "today";
   Map_1_Key_2 : aliased constant String := "current-time";
   Map_1_Key_3 : aliased constant String := "current-date";
   Map_1_Key_4 : aliased constant String := "cookies";
   Map_1_Key_5 : aliased constant String := "cookie-table";
   Map_1_Key_6 : aliased constant String := "comment-cookie-filter";
   Map_1_Key_7 : aliased constant String := "comment-cookie-link";
   Map_1_Key_8 : aliased constant String := "comment-cookie-mail";
   Map_1_Key_9 : aliased constant String := "comment-cookie-name";
   Map_1_Key_10 : aliased constant String := "element";
   Map_1_Key_11 : aliased constant String := "render";
   Map_1_Key_12 : aliased constant String := "element-or-template";
   Map_1_Key_13 : aliased constant String := "filter";
   Map_1_Key_14 : aliased constant String := "if-has-element";
   Map_1_Key_15 : aliased constant String := "if-has-not-element";
   Map_1_Key_16 : aliased constant String := "if-parameter-is";
   Map_1_Key_17 : aliased constant String := "load-date";
   Map_1_Key_18 : aliased constant String := "optional-pagelist";
   Map_1_Key_19 : aliased constant String := "optional-page-list";
   Map_1_Key_20 : aliased constant String := "optional-tag";
   Map_1_Key_21 : aliased constant String := "optional-tags";
   Map_1_Key_22 : aliased constant String := "parameter";
   Map_1_Key_23 : aliased constant String := "mime-type";
   Map_1_Key_24 : aliased constant String := "pagelist";
   Map_1_Key_25 : aliased constant String := "page-list";
   Map_1_Key_26 : aliased constant String := "tag";
   Map_1_Key_27 : aliased constant String := "tags";
   Map_1_Key_28 : aliased constant String := "template";
   Map_1_Key_29 : aliased constant String := "user";
   Map_1_Keys : constant array (0 .. 29) of access constant String
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
         Map_1_Key_20'Access,
         Map_1_Key_21'Access,
         Map_1_Key_22'Access,
         Map_1_Key_23'Access,
         Map_1_Key_24'Access,
         Map_1_Key_25'Access,
         Map_1_Key_26'Access,
         Map_1_Key_27'Access,
         Map_1_Key_28'Access,
         Map_1_Key_29'Access);
   Map_1_Elements : constant array (0 .. 29) of Command
     := (Current_Time,
         Current_Time,
         Current_Time,
         Current_Time,
         Cookies,
         Cookies,
         Comment_Cookie_Filter,
         Comment_Cookie_Link,
         Comment_Cookie_Mail,
         Comment_Cookie_Name,
         Element,
         Element_Or_Template,
         Element_Or_Template,
         Filter,
         If_Has_Element,
         If_Has_Not_Element,
         If_Parameter_Is,
         Load_Date,
         Optional_Tags,
         Optional_Tags,
         Optional_Tags,
         Optional_Tags,
         Parameter,
         Set_MIME_Type,
         Tags,
         Tags,
         Tags,
         Tags,
         Template,
         User);

end Natools.Static_Maps.Web.Fallback_Render;
