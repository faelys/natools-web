with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 7);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (12, 22, 5);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (2, 28, 11);

   G : constant array (0 .. 36) of Unsigned_8 :=
     (0, 3, 0, 0, 0, 17, 0, 13, 9, 0, 0, 0, 9, 13, 15, 0, 13, 6, 0, 0, 0,
      13, 17, 14, 0, 1, 0, 11, 0, 0, 11, 0, 15, 0, 0, 7, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 37;
         F2 := (F2 + Natural (T2 (K)) * J) mod 37;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 18;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Commands;
