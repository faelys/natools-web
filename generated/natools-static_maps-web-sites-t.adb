--  Generated at 2017-03-29 14:57:31 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-sites-maps.sx

with Natools.Static_Maps.Web.Sites.Commands;
function Natools.Static_Maps.Web.Sites.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.Sites.Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.Sites.T;
