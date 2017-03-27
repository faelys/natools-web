--  Generated at 2017-03-27 17:51:44 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-acl-maps.sx

package Natools.Static_Maps.Web.ACL is
   pragma Pure;

   type Command is
     (Unknown_Command,
      Is_In_All_Groups,
      Is_In_Any_Group,
      Is_User);

   function To_Command (Key : String) return Command;

private

   Map_1_Key_0 : aliased constant String := "is-in-groups";
   Map_1_Key_1 : aliased constant String := "is-in-all-groups";
   Map_1_Key_2 : aliased constant String := "is-in-group";
   Map_1_Key_3 : aliased constant String := "is-in-any-group";
   Map_1_Key_4 : aliased constant String := "is";
   Map_1_Key_5 : aliased constant String := "is-user";
   Map_1_Key_6 : aliased constant String := "is-any-of";
   Map_1_Keys : constant array (0 .. 6) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access);
   Map_1_Elements : constant array (0 .. 6) of Command
     := (Is_In_All_Groups,
         Is_In_All_Groups,
         Is_In_Any_Group,
         Is_In_Any_Group,
         Is_User,
         Is_User,
         Is_User);

end Natools.Static_Maps.Web.ACL;
