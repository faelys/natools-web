--  Generated at 2015-03-03 23:00:05 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-tag_pages-maps.sx

package Natools.Static_Maps.Web.Tag_Pages is
   pragma Pure;

   type Command is
     (Unknown_Command,
      Current_Tag,
      If_Index,
      If_Slash_Index,
      If_Slashless_Index,
      If_Tag,
      Root_Tag);

   type Component is
     (Unknown_Component,
      Elements,
      Index_Elements,
      No_Slash_Index,
      No_Slash_Redirect,
      No_Slashless_Index,
      No_Slashless_Redirect,
      Root,
      Slash_Index_Elements,
      Slash_Redirect,
      Slashless_Index_Elements,
      Slashless_Redirect,
      Tag_Elements);

   function To_Command (Key : String) return Command;
   function To_Component (Key : String) return Component;

private

   Map_1_Key_0 : aliased constant String := "current-tag";
   Map_1_Key_1 : aliased constant String := "current";
   Map_1_Key_2 : aliased constant String := "if-index";
   Map_1_Key_3 : aliased constant String := "if-slash-index";
   Map_1_Key_4 : aliased constant String := "if-slashless-index";
   Map_1_Key_5 : aliased constant String := "if-tag";
   Map_1_Key_6 : aliased constant String := "if-child";
   Map_1_Key_7 : aliased constant String := "root-tag";
   Map_1_Key_8 : aliased constant String := "root";
   Map_1_Keys : constant array (0 .. 8) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access);
   Map_1_Elements : constant array (0 .. 8) of Command
     := (Current_Tag,
         Current_Tag,
         If_Index,
         If_Slash_Index,
         If_Slashless_Index,
         If_Tag,
         If_Tag,
         Root_Tag,
         Root_Tag);

   Map_2_Key_0 : aliased constant String := "elements";
   Map_2_Key_1 : aliased constant String := "index-elements";
   Map_2_Key_2 : aliased constant String := "no-slash";
   Map_2_Key_3 : aliased constant String := "no-slash-index";
   Map_2_Key_4 : aliased constant String := "no-slash-redirect";
   Map_2_Key_5 : aliased constant String := "no-slashless";
   Map_2_Key_6 : aliased constant String := "no-slashless-index";
   Map_2_Key_7 : aliased constant String := "no-slashless-redirect";
   Map_2_Key_8 : aliased constant String := "root";
   Map_2_Key_9 : aliased constant String := "root-tag";
   Map_2_Key_10 : aliased constant String := "slash-elements";
   Map_2_Key_11 : aliased constant String := "slash-index-elements";
   Map_2_Key_12 : aliased constant String := "slash-redirect";
   Map_2_Key_13 : aliased constant String := "redirect-slash-index";
   Map_2_Key_14 : aliased constant String := "redirect-index-to-slashless";
   Map_2_Key_15 : aliased constant String := "redirect-to-slashless";
   Map_2_Key_16 : aliased constant String := "slashless-elements";
   Map_2_Key_17 : aliased constant String := "slashless-index-elements";
   Map_2_Key_18 : aliased constant String := "slashless-redirect";
   Map_2_Key_19 : aliased constant String := "redirect-slashless-index";
   Map_2_Key_20 : aliased constant String := "redirect-index-to-slash";
   Map_2_Key_21 : aliased constant String := "redirect-to-slash";
   Map_2_Key_22 : aliased constant String := "tag-elements";
   Map_2_Keys : constant array (0 .. 22) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access,
         Map_2_Key_5'Access,
         Map_2_Key_6'Access,
         Map_2_Key_7'Access,
         Map_2_Key_8'Access,
         Map_2_Key_9'Access,
         Map_2_Key_10'Access,
         Map_2_Key_11'Access,
         Map_2_Key_12'Access,
         Map_2_Key_13'Access,
         Map_2_Key_14'Access,
         Map_2_Key_15'Access,
         Map_2_Key_16'Access,
         Map_2_Key_17'Access,
         Map_2_Key_18'Access,
         Map_2_Key_19'Access,
         Map_2_Key_20'Access,
         Map_2_Key_21'Access,
         Map_2_Key_22'Access);
   Map_2_Elements : constant array (0 .. 22) of Component
     := (Elements,
         Index_Elements,
         No_Slash_Index,
         No_Slash_Index,
         No_Slash_Redirect,
         No_Slashless_Index,
         No_Slashless_Index,
         No_Slashless_Redirect,
         Root,
         Root,
         Slash_Index_Elements,
         Slash_Index_Elements,
         Slash_Redirect,
         Slash_Redirect,
         Slash_Redirect,
         Slash_Redirect,
         Slashless_Index_Elements,
         Slashless_Index_Elements,
         Slashless_Redirect,
         Slashless_Redirect,
         Slashless_Redirect,
         Slashless_Redirect,
         Tag_Elements);

end Natools.Static_Maps.Web.Tag_Pages;
