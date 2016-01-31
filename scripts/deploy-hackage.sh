#!/bin/bash
(echo -e "AlexanderThiemann\n$HACKAGE_PASS" && cat) | stack upload .
