with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 7);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (4, 27, 9);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (17, 10, 7);

   G : constant array (0 .. 35) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 0, 3, 0, 12, 15, 0, 0, 15, 9, 13, 6, 0, 9, 0, 4, 0, 0,
      0, 2, 1, 0, 13, 0, 1, 0, 0, 0, 16, 0, 15, 8);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 36;
         F2 := (F2 + Natural (T2 (K)) * J) mod 36;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 17;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Commands;
