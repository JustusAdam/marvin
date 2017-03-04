STACKARGS=""

echo "$STACK_RESOLVER"

if [ -n "$STACK_RESOLVER" ] 
then
    STACKARGS="$STACKARGS --resolver $STACK_RESOLVER"
fi

PACKAGES="hspec temporary process monad-loops extra yaml lens-aeson"

case "${BUILD:-stack}" in
    stack)
        stack install $STACKARGS $PACKAGES
        stack runhaskell $STACKARGS etc/IntegrationSpec.hs
    ;;
    cabal)
        cabal install $CABALARGS $PACKAGES
        runhaskell $CABALARGS etc/IntegrationSpec.hs
    ;;
esac
