--  Generated at 2014-12-25 22:09:31 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-simple_pages-maps.sx

package Natools.Static_Maps.Web.Simple_Pages is
   pragma Pure;

   type Command is
     (Unknown_Command,
      Element,
      Element_Or_Template,
      My_Tags,
      Tags,
      Template);

   type Component is
     (Error,
      Elements,
      Tags);

   function To_Command (Key : String) return Command;
   function To_Component (Key : String) return Component;

private

   Map_1_Key_0 : aliased constant String := "element";
   Map_1_Key_1 : aliased constant String := "render";
   Map_1_Key_2 : aliased constant String := "element-or-template";
   Map_1_Key_3 : aliased constant String := "my-tags";
   Map_1_Key_4 : aliased constant String := "my-tag-list";
   Map_1_Key_5 : aliased constant String := "tag-list";
   Map_1_Key_6 : aliased constant String := "pagelist";
   Map_1_Key_7 : aliased constant String := "page-list";
   Map_1_Key_8 : aliased constant String := "tag";
   Map_1_Key_9 : aliased constant String := "tags";
   Map_1_Key_10 : aliased constant String := "template";
   Map_1_Keys : constant array (0 .. 10) of access constant String
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
         Map_1_Key_10'Access);
   Map_1_Elements : constant array (0 .. 10) of Command
     := (Element,
         Element_Or_Template,
         Element_Or_Template,
         My_Tags,
         My_Tags,
         My_Tags,
         Tags,
         Tags,
         Tags,
         Tags,
         Template);

   Map_2_Key_0 : aliased constant String := "tags";
   Map_2_Key_1 : aliased constant String := "elements";
   Map_2_Key_2 : aliased constant String := "layout";
   Map_2_Keys : constant array (0 .. 2) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access);
   Map_2_Elements : constant array (0 .. 2) of Component
     := (Tags,
         Elements,
         Elements);

end Natools.Static_Maps.Web.Simple_Pages;
