with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Conditions is

   P : constant array (0 .. 4) of Natural :=
     (1, 2, 12, 16, 18);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (18, 14, 7, 27, 14);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (19, 12, 22, 5, 2);

   G : constant array (0 .. 36) of Unsigned_8 :=
     (0, 6, 0, 0, 0, 0, 0, 0, 4, 0, 0, 6, 17, 0, 0, 0, 9, 12, 3, 13, 0, 0,
      0, 4, 0, 15, 14, 2, 0, 5, 0, 7, 16, 1, 0, 6, 0);

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

end Natools.Static_Maps.Web.Comments.Item_Conditions;
