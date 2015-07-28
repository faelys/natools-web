--  Generated at 2015-07-28 19:04:59 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-list_templates-maps.sx

with Natools.Static_Maps.Web.List_Templates.Commands;
function Natools.Static_Maps.Web.List_Templates.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.List_Templates.Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.List_Templates.T;
