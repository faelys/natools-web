with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 3) of Natural :=
     (1, 4, 8, 9);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (0, 34, 33, 36);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (38, 19, 3, 3);

   G : constant array (0 .. 38) of Unsigned_8 :=
     (0, 16, 8, 0, 13, 0, 0, 0, 5, 0, 9, 2, 0, 0, 5, 0, 0, 2, 0, 0, 7, 2,
      18, 0, 0, 12, 0, 0, 0, 0, 3, 5, 0, 0, 0, 7, 3, 0, 4);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 39;
         F2 := (F2 + Natural (T2 (K)) * J) mod 39;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 19;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
