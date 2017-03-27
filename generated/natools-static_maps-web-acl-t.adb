--  Generated at 2017-03-27 17:51:45 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-acl-maps.sx

with Natools.Static_Maps.Web.ACL.Commands;
function Natools.Static_Maps.Web.ACL.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.ACL.Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.ACL.T;
