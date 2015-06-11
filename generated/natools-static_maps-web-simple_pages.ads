--  Generated at 2015-06-11 16:57:49 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-simple_pages-maps.sx

package Natools.Static_Maps.Web.Simple_Pages is
   pragma Pure;

   type Command is
     (Unknown_Command,
      Comment_List,
      Date, Optional_Date, If_No_Date,
      My_Tags,
      Path,
      Tags);

   type Component is
     (Error,
      Comment_List,
      Dates,
      Elements,
      Tags);

   function To_Command (Key : String) return Command;
   function To_Component (Key : String) return Component;

private

   Map_1_Key_0 : aliased constant String := "comments";
   Map_1_Key_1 : aliased constant String := "comment-list";
   Map_1_Key_2 : aliased constant String := "date";
   Map_1_Key_3 : aliased constant String := "if-no-date";
   Map_1_Key_4 : aliased constant String := "my-tags";
   Map_1_Key_5 : aliased constant String := "my-tag-list";
   Map_1_Key_6 : aliased constant String := "tag-list";
   Map_1_Key_7 : aliased constant String := "optional-date";
   Map_1_Key_8 : aliased constant String := "path";
   Map_1_Key_9 : aliased constant String := "pagelist";
   Map_1_Key_10 : aliased constant String := "page-list";
   Map_1_Key_11 : aliased constant String := "tag";
   Map_1_Key_12 : aliased constant String := "tags";
   Map_1_Keys : constant array (0 .. 12) of access constant String
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
         Map_1_Key_12'Access);
   Map_1_Elements : constant array (0 .. 12) of Command
     := (Comment_List,
         Comment_List,
         Date,
         If_No_Date,
         My_Tags,
         My_Tags,
         My_Tags,
         Optional_Date,
         Path,
         Tags,
         Tags,
         Tags,
         Tags);

   Map_2_Key_0 : aliased constant String := "dates";
   Map_2_Key_1 : aliased constant String := "tags";
   Map_2_Key_2 : aliased constant String := "comments";
   Map_2_Key_3 : aliased constant String := "comment-list";
   Map_2_Key_4 : aliased constant String := "elements";
   Map_2_Key_5 : aliased constant String := "layout";
   Map_2_Keys : constant array (0 .. 5) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access,
         Map_2_Key_5'Access);
   Map_2_Elements : constant array (0 .. 5) of Component
     := (Dates,
         Tags,
         Comment_List,
         Comment_List,
         Elements,
         Elements);

end Natools.Static_Maps.Web.Simple_Pages;
