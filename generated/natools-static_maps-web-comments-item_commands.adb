with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 7);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (0, 9, 26);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (29, 15, 18);

   G : constant array (0 .. 29) of Unsigned_8 :=
     (0, 0, 0, 3, 7, 0, 0, 0, 0, 0, 4, 0, 9, 0, 11, 1, 4, 0, 3, 0, 0, 9, 0,
      11, 6, 8, 0, 7, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 30;
         F2 := (F2 + Natural (T2 (K)) * J) mod 30;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 14;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Commands;
