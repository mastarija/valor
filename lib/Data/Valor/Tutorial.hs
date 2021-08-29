module Data.Valor.Tutorial
  (
  -- * Tutorial
  -- $tutorial
  ) where
--
import Data.Valor
--

{- $tutorial

  Let's say we want to validate an application form for a team competition in
  which teams from different countries compete.

  We want each application to consist of a:

  * team name
  * team country
  * team captain
  * team members

  == Example domain

  Here's how our domain might look for such usecase:

  >>> :{
    data State = State
      { teams     :: [String]
      , countries :: [String]
      } deriving ( Eq , Show )
  :}

  >>> :{
    newtype Age = Age
      { unAge :: Int
      } deriving ( Eq , Show )
  :}

  >>> :{
    newtype Team = Team
      { unTeam :: String
      } deriving ( Eq , Show )
  :}

  >>> :{
    newtype Email = Email
      { unEmail :: String
      } deriving ( Eq , Show )
  :}

  >>> :{
    newtype Country = Country
      { unCountry :: String
      } deriving ( Eq , Show )
  :}

  >>> :{
    data Participant = Participant
      { age     :: Age
      , name    :: String
      , surname :: String
      , email   :: Email
      } deriving ( Eq , Show )
  :}

  >>> :{
    data Application = Application
      { team    :: Team
      , country :: Country
      , captain :: Participant
      , members :: [Participant]
      } deriving ( Eq , Show )
  :}

  The @State@ data type will represent our \"database\" in which we will check
  if the team with a certain name is already registered, or if applicants
  country is on allowed country list.

  We've created a few @newtype@s to make it clear what we are validating. Let's
  say we want to limit participants age within a certain range.

  == Error values

  First we will define another data type for errors that can occur during the
  age validation process. Age will be limited between 18 and 65 years, meaning
  our applicants can be over or under age:

  >>> :{
    data AgeError = AgeUnder | AgeOver
      deriving ( Eq , Show )
  :}

  == Simple validators

  Now we can work on constructing our validator. If we want our applicants to be
  over 18 years old we can write @'passIf' \[AgeUnder\] (>18)@. Similarly, we
  can restrict the age to under 65 by writing @'failIf' \[AgeOver\] (>65)@.
  Because 'Valor' is a 'Monoid' we can combine two validators into one like
  this:

  >>> :{
    ageV :: Monad m => Valor Age m [ AgeError ]
    ageV = adapt unAge $ passIf [ AgeUnder ] (>18) <> failIf [ AgeOver ] (>65)
  :}

  Here we've used the 'adapt' function to adapt our simple validators that work
  with plain integers to the @Age@ @newtype@ that wraps an 'Int' value. This way
  we can write @(>18)@ instead of @((>18) . unAge)@ in our validation predicate.

  Let's write a validator for @Team@. We don't want the team name to be empty,
  shorter than 4 letters, longer than 50 or already registered. To make those
  cases clearer, here's the @TeamError@:

  >>> :{
    data TeamError = TeamEmpty | TeamShort | TeamLong | TeamTaken
      deriving ( Eq , Show )
  :}

  == Adapting validators and monadic checks

  We'll use the 'adapt' function again to simplify our validator construction,
  along with the 'mconcat' which will allow us to avoid manually combining
  validators with '<>':

  >>> :{
    teamV :: Valor Team ( (->) State ) [ TeamError ]
    teamV = adapt unTeam $ mconcat
      [ failIf [ TeamEmpty ] null
      , passIf [ TeamShort ] ((>3) . length)
      , failIf [ TeamLong ] ((>50) . length)
      , make $ \ i -> do
          ts <- teams
          pure $ if i `elem` ts
            then Just [ TeamTaken ]
            else Nothing
      ]
  :}

  Here we are using the @'->' r@ 'Monad' which is essentially just a reader
  monad. It simulates our database in which we can check for already registered
  teams, or allowed countries.

  Instead of 'failIf' and 'passIf' the 'make' function was used to construct a
  validator that checks if the team was already registered, as it allows us to
  perform 'Monad'ic computation. There are also 'failIfM' and 'passIfM' which
  also allow us to perform a 'Monad'ic computation.

  Here's another simple example of constructing a very basic validator for
  'Email':

  >>> :{
    data EmailError = EmailEmpty | EmailNoAt | EmailNoDot
      deriving ( Eq , Show )
  :}

  >>> :{
    emailV :: Monad m => Valor Email m [ EmailError ]
    emailV = adapt unEmail $ mconcat
      [ failIf [ EmailEmpty ] null
      , passIf [ EmailNoAt ] (any (=='@'))
      , passIf [ EmailNoDot ] (any (=='.'))
      ]
  :}

  And another 'Monad'ic example checking if the @Country@ is allowed:

  >>> :{
    data CountryError = CountryEmpty | CountryNotAllowed
      deriving ( Eq , Show )
  :}

  >>> :{
    countryV :: Valor Country ( (->) State ) [ CountryError ]
    countryV = adapt unCountry $ mconcat
      [ failIf [ CountryEmpty ] null
      , make $ \ i -> do
          cs <- countries
          pure $ if i `elem` cs
            then Nothing
            else Just [ CountryNotAllowed ]
      ]
  :}

  == Structured errors

  Now let's try to create validate a more complex data type like @Participant@:

  @
  data Participant = Participant
    { age     :: Age
    , name    :: String
    , surname :: String
    , email   :: Email
    } deriving ( Eq , Show )
  @

  It has many fields of different data types. For each field we'd like to know
  if it has failed, and how. That way we can report to the user where exactly
  is the error and what it is. To do so, let's construct the @ParticipantError@
  record data type which will mirror the @Participant@:

  >>> :{
    data ParticipantError = ParticipantError
      { ageE      :: Maybe [ AgeError ]
      , nameE     :: Maybe [ String ]
      , surnameE  :: Maybe [ String ]
      , emailE    :: Maybe [ EmailError ]
      } deriving ( Eq , Show )
  :}

  Notice how each field has 'Maybe', this is because each individual field can
  be valid or invalid. If there is no error, then we will get 'Nothing',
  otherwise we'll get 'Just' the error value from a \"sub\" validator.

  Here's how we can construct our @Participant@ validator using 'check1' and
  previously defined validators along with some ad-hoc validators:

  >>> :{
    participantV :: Monad m => Valor Participant m ParticipantError
    participantV = ParticipantError
      <$> check1 age ageV
      <*> check1 name (failIf ["name can't be empty"] null)
      <*> check1 surname (failIf ["surname can't be empty"] null)
      <*> check1 email emailV
  :}

  We can use 'checkN' to validate every value in a list. Let's put together a
  validator for the @Application@. Similarly to the @Participant@, we first
  define the @ApplicationError@ to store our @Application@ errors:

  >>> :{
    data ApplicationError = ApplicationError
      { teamE     :: Maybe [ TeamError ]
      , countryE  :: Maybe [ CountryError ]
      , captainE  :: Maybe ParticipantError
      , membersE  :: Maybe [ Maybe ParticipantError ]
      } deriving ( Eq , Show )
  :}

  Notice that @membersE@ field is of type @Maybe [ Maybe ParticipantError ]@.
  This way, if even a single participant is erroneous we get back a 'Just' a
  list of 'Maybe's where 'Nothing' represents no error on that position in a
  list and 'Just' states that error occured on that element in a list.

  Finally, this is how we construct the @Application@ validator:

  >>> :{
    applicationV :: Valor Application ( (->) State ) ApplicationError
    applicationV = ApplicationError
      <$> check1 team teamV
      <*> check1 country countryV
      <*> check1 captain participantV
      <*> checkN members participantV
  :}

  And because we are using the @countryV@ we have to fix our 'Monad' to
  @(->) State@.

  == Usage examples

  Now we can create some test data and check out our validation results. Here is
  our \"database\":

  >>> :{
    state :: State
    state = State
      { teams = [ "Taken" ]
      , countries = [ "Croatia" , "Germany" , "USA" , "Japan" ]
      }
  :}

  a few participants:

  >>> :{
    exParticipantValid1 :: Participant
    exParticipantValid1 = Participant
      { age = Age 30
      , name = "Pero"
      , surname = "Perić"
      , email = Email "pero.peric@email.com"
      }
  :}

  >>> :{
    exParticipantValid2 :: Participant
    exParticipantValid2 = Participant
      { age = Age 51
      , name = "Marko"
      , surname = "Marić"
      , email = Email "marko.maric@email.com"
      }
  :}

  >>> :{
    exParticipantValid3 :: Participant
    exParticipantValid3 = Participant
      { age = Age 29
      , name = "Jane"
      , surname = "Doe"
      , email = Email "jane.doe@email.com"
      }
  :}

  >>> :{
    exParticipantInvalid1 :: Participant
    exParticipantInvalid1 = Participant
      { age = Age 48
      , name = ""
      , surname = "Perić"
      , email = Email "peropericemailcom"
      }
  :}

  >>> :{
    exParticipantInvalid2 :: Participant
    exParticipantInvalid2 = Participant
      { age = Age 73
      , name = "John"
      , surname = "Doe"
      , email = Email "john.doe@mail.com"
      }
  :}

  >>> :{
    exParticipantInvalid3 :: Participant
    exParticipantInvalid3 = Participant
      { age = Age 17
      , name = "Mini"
      , surname = "Morris"
      , email = Email ""
      }
  :}

  and finally some applications:

  >>> :{
    exApplicationValid :: Application
    exApplicationValid = Application
      { team = Team "Valor"
      , country = Country "Croatia"
      , captain = exParticipantValid1
      , members = [ exParticipantValid2 , exParticipantValid3 ]
      }
  :}

  >>> :{
    exApplicationInvalid1 :: Application
    exApplicationInvalid1 = Application
      { team = Team "Taken"
      , country = Country ""
      , captain = exParticipantValid1
      , members = [ exParticipantInvalid1 , exParticipantValid3 ]
      }
  :}

  >>> :{
    exApplicationInvalid2 :: Application
    exApplicationInvalid2 = Application
      { team = Team "srt"
      , country = Country "Murica!"
      , captain = exParticipantInvalid1
      , members = [ exParticipantInvalid2 , exParticipantValid1 , exParticipantValid3 , exParticipantValid2 ]
      }
  :}

  And we can check the results

  >>> validateM applicationV exApplicationValid state
  Left (Valid (Application {team = Team {unTeam = "Valor"}, country = Country {unCountry = "Croatia"}, captain = Participant {age = Age {unAge = 30}, name = "Pero", surname = "Peri\263", email = Email {unEmail = "pero.peric@email.com"}}, members = [Participant {age = Age {unAge = 51}, name = "Marko", surname = "Mari\263", email = Email {unEmail = "marko.maric@email.com"}},Participant {age = Age {unAge = 29}, name = "Jane", surname = "Doe", email = Email {unEmail = "jane.doe@email.com"}}]}))

  >>> validateM applicationV exApplicationInvalid1 state
  Right (ApplicationError {teamE = Just [TeamTaken], countryE = Just [CountryEmpty,CountryNotAllowed], captainE = Nothing, membersE = Just [Just (ParticipantError {ageE = Nothing, nameE = Just ["name can't be empty"], surnameE = Nothing, emailE = Just [EmailNoAt,EmailNoDot]}),Nothing]})

  >>> validateM applicationV exApplicationInvalid2 state
  Right (ApplicationError {teamE = Just [TeamShort], countryE = Just [CountryNotAllowed], captainE = Just (ParticipantError {ageE = Nothing, nameE = Just ["name can't be empty"], surnameE = Nothing, emailE = Just [EmailNoAt,EmailNoDot]}), membersE = Just [Just (ParticipantError {ageE = Just [AgeOver], nameE = Nothing, surnameE = Nothing, emailE = Nothing}),Nothing,Nothing,Nothing]})

  That's all folks!
-}
