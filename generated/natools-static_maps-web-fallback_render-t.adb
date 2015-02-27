--  Generated at 2015-02-27 22:21:51 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-fallback_render-maps.sx

with Natools.Static_Maps.Web.Fallback_Render.Commands;
function Natools.Static_Maps.Web.Fallback_Render.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.Fallback_Render.Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.Fallback_Render.T;
