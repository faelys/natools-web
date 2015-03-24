with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Commands is

   P : constant array (0 .. 1) of Natural :=
     (1, 2);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (11, 3);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (3, 14);

   G : constant array (0 .. 22) of Unsigned_8 :=
     (8, 0, 0, 0, 3, 0, 0, 0, 8, 2, 0, 0, 0, 1, 1, 9, 0, 0, 3, 0, 0, 7, 6);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 23;
         F2 := (F2 + Natural (T2 (K)) * J) mod 23;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 11;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Commands;
