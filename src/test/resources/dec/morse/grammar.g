start := @String (morse (" " (@Char @CharValue " ")? morse)*);

morse := @Char (
      [-]
      ( [-]
        ( [-]
          ( [-] (
              [-]    @CharValue{"0"}()
            | [.]    @CharValue{"9"}()
            )
          | [.][.]   @CharValue{"8"}()
          |          @CharValue{"o"}()
          )
        | [.]
          ( [.] (
              [-]    @CharValue{"q"}()
            | [.]    @CharValue{"7"}()
            |        @CharValue{"z"}()
            )
          |          @CharValue{"g"}()
          )
        |            @CharValue{"m"}()
        )
      | [.]
        ( [-]
          ( [-]      @CharValue{"y"}()
          | [.]      @CharValue{"c"}()
          |          @CharValue{"k"}()
          )
        | [.]
          ( [-]      @CharValue{"x"}()
          | [.] (
              [.]    @CharValue{"6"}()
            |        @CharValue{"b"}()
            )
          |          @CharValue{"d"}()   
          )
        |            @CharValue{"n"}()
        )
      |              @CharValue{"t"}()
      )
    | [.]
      ( [-]
        ( [-]
          ( [-]
            ( [-]    @CharValue{"1"}()
            |        @CharValue{"j"}()
            )
          | [.]      @CharValue{"p"}()
          |          @CharValue{"w"}()
          )
        | [.]
          ( [.]      @CharValue{"l"}()
          |          @CharValue{"r"}()
          )
        |            @CharValue{"a"}()
        )
      | [.]
        ( [-]
          ( [-][-]   @CharValue{"2"}()
          | [.]      @CharValue{"f"}()
          |          @CharValue{"u"}()
          )
        | [.]
          ( [-]
            ( [-]    @CharValue{"3"}()   
            |        @CharValue{"v"}()
            )
          | [.]
            ( [-]    @CharValue{"4"}()
            | [.]    @CharValue{"5"}()
            |        @CharValue{"h"}()
            )
          |          @CharValue{"s"}()
          )
        |            @CharValue{"i"}()
        )
      |              @CharValue{"e"}()
      )
    );
