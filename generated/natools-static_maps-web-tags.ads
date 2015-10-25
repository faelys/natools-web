--  Generated at 2015-10-25 22:37:42 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-tags-maps.sx

package Natools.Static_Maps.Web.Tags is
   pragma Pure;

   type List_Command is
     (Unknown_List_Command,
      All_Children,
      All_Descendants,
      All_Elements,
      All_Leaves,
      Current_Element,
      Current_Tag_List,
      Element_Count,
      Greater_Children,
      Greater_Descendants,
      Greater_Elements,
      Greater_Leaves,
      If_Current,
      If_Current_Leaf,
      If_Current_Parent,
      If_Not_Current,
      If_Not_Current_Leaf,
      If_Not_Current_Parent,
      Full_Name,
      Last_Name,
      Lesser_Children,
      Lesser_Descendants,
      Lesser_Elements,
      Lesser_Leaves,
      Name,
      Parent,
      Tags);

   function To_List_Command (Key : String) return List_Command;

private

   Map_1_Key_0 : aliased constant String := "all-children";
   Map_1_Key_1 : aliased constant String := "all-descendants";
   Map_1_Key_2 : aliased constant String := "all-elements";
   Map_1_Key_3 : aliased constant String := "all-leaves";
   Map_1_Key_4 : aliased constant String := "current-element";
   Map_1_Key_5 : aliased constant String := "current-tag-list";
   Map_1_Key_6 : aliased constant String := "element-count";
   Map_1_Key_7 : aliased constant String := "greater-children";
   Map_1_Key_8 : aliased constant String := "greater-descendants";
   Map_1_Key_9 : aliased constant String := "greater-elements";
   Map_1_Key_10 : aliased constant String := "greater-leaves";
   Map_1_Key_11 : aliased constant String := "if-current";
   Map_1_Key_12 : aliased constant String := "if-current-leaf";
   Map_1_Key_13 : aliased constant String := "if-current-parent";
   Map_1_Key_14 : aliased constant String := "if-not-current";
   Map_1_Key_15 : aliased constant String := "if-not-current-leaf";
   Map_1_Key_16 : aliased constant String := "if-not-current-parent";
   Map_1_Key_17 : aliased constant String := "full-name";
   Map_1_Key_18 : aliased constant String := "last-name";
   Map_1_Key_19 : aliased constant String := "lesser-children";
   Map_1_Key_20 : aliased constant String := "lesser-descendants";
   Map_1_Key_21 : aliased constant String := "lesser-elements";
   Map_1_Key_22 : aliased constant String := "lesser-leaves";
   Map_1_Key_23 : aliased constant String := "name";
   Map_1_Key_24 : aliased constant String := "parent";
   Map_1_Key_25 : aliased constant String := "tags";
   Map_1_Keys : constant array (0 .. 25) of access constant String
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
         Map_1_Key_25'Access);
   Map_1_Elements : constant array (0 .. 25) of List_Command
     := (All_Children,
         All_Descendants,
         All_Elements,
         All_Leaves,
         Current_Element,
         Current_Tag_List,
         Element_Count,
         Greater_Children,
         Greater_Descendants,
         Greater_Elements,
         Greater_Leaves,
         If_Current,
         If_Current_Leaf,
         If_Current_Parent,
         If_Not_Current,
         If_Not_Current_Leaf,
         If_Not_Current_Parent,
         Full_Name,
         Last_Name,
         Lesser_Children,
         Lesser_Descendants,
         Lesser_Elements,
         Lesser_Leaves,
         Name,
         Parent,
         Tags);

end Natools.Static_Maps.Web.Tags;
