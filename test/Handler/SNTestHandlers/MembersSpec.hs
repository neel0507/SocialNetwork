{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SNTestHandlers.MembersSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getMembersR" $ do
        it "Accessing members page with two site users" $ do
          (uname1, password1) <- doSignup "Neel1" "1234"
          (uname2, password2) <- doSignup "Neel2" "12345"
          doLogin uname1 password1
          get MembersR
          statusIs 200
          bodyContains "Other Members"
          bodyContains "Neel2"

    describe "viewMemberR" $ do
        it "Viewing a member profile through members page" $ do
          (uname1, password1) <- doSignup "Neel1" "1234"
          (uname2, password2) <- doSignup "Neel2" "12345"             
          doLogin uname1 password1
          get MembersR
          statusIs 200    
          get (ViewMemberR uname2)
          bodyContains "Neel2&#39;s Profile" --Neel2's Profile

    describe "getMembersRFollow" $ do
        it "following a member through members page" $ do
          (uname1, password1) <- doSignup "Neel1" "1234" --id:1
          (uname2, password2) <- doSignup "Neel2" "12345" --id:2      
          doLogin uname1 password1
          get MembersR
          statusIs 200
          request $ do
             setUrl ("http://localhost:3000/members?add=2" :: Text) --id:2
          bodyContains "Neel2</a> &larr; you are following" --Neel2 ← you are following

    describe "getMembersRFollowDrop" $ do --User goes to members page, then follows a member and then drop a member
        it "following a member through members page and then dropping the member" $ do
          (uname1, password1) <- doSignup "Neel1" "1234" --id:1
          (uname2, password2) <- doSignup "Neel2" "12345" --id:2           
          doLogin uname1 password1
          get MembersR
          statusIs 200
          request $ do
             setUrl ("http://localhost:3000/members?add=2" :: Text) --id:2
          bodyContains "Neel2</a> &larr; you are following" --Neel2 ← you are following
          request $ do
             setUrl ("http://localhost:3000/members?remove=2" :: Text) --id:2
          followingMembers <- runDB $ selectList ([] :: [Filter FollowingMembers]) []
          assertEq "Following members table empty" 0 $ length followingMembers

    describe "getMembersRFollowLogoutLoginFollow" $ do
        it "following a member through members page and then logging out and then logging in as another member, following the follower and then accessing friends page" $ do
          (uname1, password1) <- doSignup "Neel1" "1234" --id:1
          (uname2, password2) <- doSignup "Neel2" "12345" --id:2  
          doLogin uname1 password1
          get MembersR
          statusIs 200
          request $ do
             setUrl ("http://localhost:3000/members?add=2" :: Text) --id:2
          bodyContains "Neel2</a> &larr; you are following" --Neel2 ← you are following
          get LogoutpageR
          doLogin uname2 password2
          get MembersR
          statusIs 200
          followingMembers <- runDB $ selectList ([] :: [Filter FollowingMembers]) []
          assertEq "Following members table has one record" 1 $ length followingMembers
          bodyContains "Neel"
          request $ do
             setUrl ("http://localhost:3000/members?add=1" :: Text) --id:1
          followingMembers <- runDB $ selectList ([] :: [Filter FollowingMembers]) []
          assertEq "Following members table has two records" 2 $ length followingMembers
          get FriendsR
          htmlAnyContain "h2" "Mutual Friends"
          htmlAnyContain "li" "Neel"
          htmlAnyContain "h2" "You are not following anyone yet, they might be your mutual friend"
          htmlAnyContain "h2" "You are not followed by anyone yet, they might be your mutual friend"
