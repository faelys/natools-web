--  Generated at 2015-06-30 20:35:41 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-tags-maps.sx

with Natools.Static_Maps.Web.Tags.Commands;
function Natools.Static_Maps.Web.Tags.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.Tags.Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.Tags.T;
