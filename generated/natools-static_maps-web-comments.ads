--  Generated at 2015-06-05 20:15:44 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-comments-maps.sx

package Natools.Static_Maps.Web.Comments is
   pragma Pure;

   package Item is
      package Command is
         type Enum is (Unknown, Date, Id, Name, Mail, Link, Text, Parent,
           If_Name, If_No_Name, If_Mail, If_No_Mail, If_Link, If_No_Link);
      end Command;

      package Condition is
         type Enum is
           (Unknown,
            Has_Extra_Fields, Has_Unknown_Field, Action_Is,
            Field_List_Is, Field_List_Contains, Field_List_Among,
            Name, Mail, Link, Text);
      end Condition;

      package Element is
         type Enum is
           (Unknown, Date, Name, Mail, Link, Text, Text_Filter, Flags);
      end Element;

      package Form is
         type Enum is (Unknown, Date, Filter, Name, Mail, Link, Text);
      end Form;

      package Post_Action is
         type Enum is
           (Unknown, Save, Force_Preview, Reject, Dump,
            Reason, Append_Reason, Set_Reason, Ignore, Unignore, Anchor);
      end Post_Action;
   end Item;

   package List is
      package Command is
         type Enum is (Unknown, List, Preview, Size);
      end Command;

      package Element is
         type Enum is
           (Unknown, Backend, Post_Filter, Tags,
            Default_Text_Filter, Text_Filters, Flags);
      end Element;
   end List;

   function To_Item_Command (Key : String) return Item.Command.Enum;
   function To_Item_Condition (Key : String) return Item.Condition.Enum;
   function To_Item_Element (Key : String) return Item.Element.Enum;
   function To_Item_Form (Key : String) return Item.Form.Enum;
   function To_Item_Action (Key : String) return Item.Post_Action.Enum;
   function To_List_Command (Key : String) return List.Command.Enum;
   function To_List_Element (Key : String) return List.Element.Enum;

private

   Map_1_Key_0 : aliased constant String := "date";
   Map_1_Key_1 : aliased constant String := "id";
   Map_1_Key_2 : aliased constant String := "if-link";
   Map_1_Key_3 : aliased constant String := "if-no-link";
   Map_1_Key_4 : aliased constant String := "if-no-mail";
   Map_1_Key_5 : aliased constant String := "if-no-name";
   Map_1_Key_6 : aliased constant String := "if-mail";
   Map_1_Key_7 : aliased constant String := "if-name";
   Map_1_Key_8 : aliased constant String := "name";
   Map_1_Key_9 : aliased constant String := "mail";
   Map_1_Key_10 : aliased constant String := "e-mail";
   Map_1_Key_11 : aliased constant String := "link";
   Map_1_Key_12 : aliased constant String := "www";
   Map_1_Key_13 : aliased constant String := "site";
   Map_1_Key_14 : aliased constant String := "website";
   Map_1_Key_15 : aliased constant String := "parent";
   Map_1_Key_16 : aliased constant String := "text";
   Map_1_Keys : constant array (0 .. 16) of access constant String
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
         Map_1_Key_16'Access);
   Map_1_Elements : constant array (0 .. 16) of Item.Command.Enum
     := (Item.Command.Date,
         Item.Command.Id,
         Item.Command.If_Link,
         Item.Command.If_No_Link,
         Item.Command.If_No_Mail,
         Item.Command.If_No_Name,
         Item.Command.If_Mail,
         Item.Command.If_Name,
         Item.Command.Name,
         Item.Command.Mail,
         Item.Command.Mail,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Parent,
         Item.Command.Text);

   Map_2_Key_0 : aliased constant String := "action-is";
   Map_2_Key_1 : aliased constant String := "action-is-any-of";
   Map_2_Key_2 : aliased constant String := "field-list-is";
   Map_2_Key_3 : aliased constant String := "field-list-contains";
   Map_2_Key_4 : aliased constant String := "field-list-among";
   Map_2_Key_5 : aliased constant String := "has-extra-field";
   Map_2_Key_6 : aliased constant String := "has-extra-fields";
   Map_2_Key_7 : aliased constant String := "has-unknown-field";
   Map_2_Key_8 : aliased constant String := "has-unknown-fields";
   Map_2_Key_9 : aliased constant String := "name";
   Map_2_Key_10 : aliased constant String := "mail";
   Map_2_Key_11 : aliased constant String := "e-mail";
   Map_2_Key_12 : aliased constant String := "link";
   Map_2_Key_13 : aliased constant String := "www";
   Map_2_Key_14 : aliased constant String := "site";
   Map_2_Key_15 : aliased constant String := "website";
   Map_2_Key_16 : aliased constant String := "text";
   Map_2_Keys : constant array (0 .. 16) of access constant String
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
         Map_2_Key_16'Access);
   Map_2_Elements : constant array (0 .. 16) of Item.Condition.Enum
     := (Item.Condition.Action_Is,
         Item.Condition.Action_Is,
         Item.Condition.Field_List_Is,
         Item.Condition.Field_List_Contains,
         Item.Condition.Field_List_Among,
         Item.Condition.Has_Extra_Fields,
         Item.Condition.Has_Extra_Fields,
         Item.Condition.Has_Unknown_Field,
         Item.Condition.Has_Unknown_Field,
         Item.Condition.Name,
         Item.Condition.Mail,
         Item.Condition.Mail,
         Item.Condition.Link,
         Item.Condition.Link,
         Item.Condition.Link,
         Item.Condition.Link,
         Item.Condition.Text);

   Map_3_Key_0 : aliased constant String := "date";
   Map_3_Key_1 : aliased constant String := "flags";
   Map_3_Key_2 : aliased constant String := "name";
   Map_3_Key_3 : aliased constant String := "mail";
   Map_3_Key_4 : aliased constant String := "e-mail";
   Map_3_Key_5 : aliased constant String := "link";
   Map_3_Key_6 : aliased constant String := "www";
   Map_3_Key_7 : aliased constant String := "site";
   Map_3_Key_8 : aliased constant String := "website";
   Map_3_Key_9 : aliased constant String := "text";
   Map_3_Key_10 : aliased constant String := "text-filter";
   Map_3_Key_11 : aliased constant String := "filter";
   Map_3_Keys : constant array (0 .. 11) of access constant String
     := (Map_3_Key_0'Access,
         Map_3_Key_1'Access,
         Map_3_Key_2'Access,
         Map_3_Key_3'Access,
         Map_3_Key_4'Access,
         Map_3_Key_5'Access,
         Map_3_Key_6'Access,
         Map_3_Key_7'Access,
         Map_3_Key_8'Access,
         Map_3_Key_9'Access,
         Map_3_Key_10'Access,
         Map_3_Key_11'Access);
   Map_3_Elements : constant array (0 .. 11) of Item.Element.Enum
     := (Item.Element.Date,
         Item.Element.Flags,
         Item.Element.Name,
         Item.Element.Mail,
         Item.Element.Mail,
         Item.Element.Link,
         Item.Element.Link,
         Item.Element.Link,
         Item.Element.Link,
         Item.Element.Text,
         Item.Element.Text_Filter,
         Item.Element.Text_Filter);

   Map_4_Key_0 : aliased constant String := "c_date";
   Map_4_Key_1 : aliased constant String := "c_filter";
   Map_4_Key_2 : aliased constant String := "c_name";
   Map_4_Key_3 : aliased constant String := "c_mail";
   Map_4_Key_4 : aliased constant String := "c_site";
   Map_4_Key_5 : aliased constant String := "c_text";
   Map_4_Keys : constant array (0 .. 5) of access constant String
     := (Map_4_Key_0'Access,
         Map_4_Key_1'Access,
         Map_4_Key_2'Access,
         Map_4_Key_3'Access,
         Map_4_Key_4'Access,
         Map_4_Key_5'Access);
   Map_4_Elements : constant array (0 .. 5) of Item.Form.Enum
     := (Item.Form.Date,
         Item.Form.Filter,
         Item.Form.Name,
         Item.Form.Mail,
         Item.Form.Link,
         Item.Form.Text);

   Map_5_Key_0 : aliased constant String := "anchor";
   Map_5_Key_1 : aliased constant String := "no-anchor";
   Map_5_Key_2 : aliased constant String := "append-reason";
   Map_5_Key_3 : aliased constant String := "add-reason";
   Map_5_Key_4 : aliased constant String := "dump";
   Map_5_Key_5 : aliased constant String := "log";
   Map_5_Key_6 : aliased constant String := "write";
   Map_5_Key_7 : aliased constant String := "preview";
   Map_5_Key_8 : aliased constant String := "force-preview";
   Map_5_Key_9 : aliased constant String := "ignore";
   Map_5_Key_10 : aliased constant String := "reason";
   Map_5_Key_11 : aliased constant String := "reject";
   Map_5_Key_12 : aliased constant String := "accept";
   Map_5_Key_13 : aliased constant String := "save";
   Map_5_Key_14 : aliased constant String := "set-reason";
   Map_5_Key_15 : aliased constant String := "reset-reason";
   Map_5_Key_16 : aliased constant String := "unignore";
   Map_5_Keys : constant array (0 .. 16) of access constant String
     := (Map_5_Key_0'Access,
         Map_5_Key_1'Access,
         Map_5_Key_2'Access,
         Map_5_Key_3'Access,
         Map_5_Key_4'Access,
         Map_5_Key_5'Access,
         Map_5_Key_6'Access,
         Map_5_Key_7'Access,
         Map_5_Key_8'Access,
         Map_5_Key_9'Access,
         Map_5_Key_10'Access,
         Map_5_Key_11'Access,
         Map_5_Key_12'Access,
         Map_5_Key_13'Access,
         Map_5_Key_14'Access,
         Map_5_Key_15'Access,
         Map_5_Key_16'Access);
   Map_5_Elements : constant array (0 .. 16) of Item.Post_Action.Enum
     := (Item.Post_Action.Anchor,
         Item.Post_Action.Anchor,
         Item.Post_Action.Append_Reason,
         Item.Post_Action.Append_Reason,
         Item.Post_Action.Dump,
         Item.Post_Action.Dump,
         Item.Post_Action.Dump,
         Item.Post_Action.Force_Preview,
         Item.Post_Action.Force_Preview,
         Item.Post_Action.Ignore,
         Item.Post_Action.Reason,
         Item.Post_Action.Reject,
         Item.Post_Action.Save,
         Item.Post_Action.Save,
         Item.Post_Action.Set_Reason,
         Item.Post_Action.Set_Reason,
         Item.Post_Action.Unignore);

   Map_6_Key_0 : aliased constant String := "list";
   Map_6_Key_1 : aliased constant String := "preview";
   Map_6_Key_2 : aliased constant String := "size";
   Map_6_Key_3 : aliased constant String := "length";
   Map_6_Keys : constant array (0 .. 3) of access constant String
     := (Map_6_Key_0'Access,
         Map_6_Key_1'Access,
         Map_6_Key_2'Access,
         Map_6_Key_3'Access);
   Map_6_Elements : constant array (0 .. 3) of List.Command.Enum
     := (List.Command.List,
         List.Command.Preview,
         List.Command.Size,
         List.Command.Size);

   Map_7_Key_0 : aliased constant String := "backend";
   Map_7_Key_1 : aliased constant String := "default-text-filter";
   Map_7_Key_2 : aliased constant String := "flags";
   Map_7_Key_3 : aliased constant String := "post-filter";
   Map_7_Key_4 : aliased constant String := "filter";
   Map_7_Key_5 : aliased constant String := "tags";
   Map_7_Key_6 : aliased constant String := "allowed-filters";
   Map_7_Key_7 : aliased constant String := "text-filters";
   Map_7_Keys : constant array (0 .. 7) of access constant String
     := (Map_7_Key_0'Access,
         Map_7_Key_1'Access,
         Map_7_Key_2'Access,
         Map_7_Key_3'Access,
         Map_7_Key_4'Access,
         Map_7_Key_5'Access,
         Map_7_Key_6'Access,
         Map_7_Key_7'Access);
   Map_7_Elements : constant array (0 .. 7) of List.Element.Enum
     := (List.Element.Backend,
         List.Element.Default_Text_Filter,
         List.Element.Flags,
         List.Element.Post_Filter,
         List.Element.Post_Filter,
         List.Element.Tags,
         List.Element.Text_Filters,
         List.Element.Text_Filters);

end Natools.Static_Maps.Web.Comments;
