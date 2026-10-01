package org.z390.test

import org.junit.jupiter.api.condition.EnabledIfSystemProperty

import java.lang.annotation.ElementType
import java.lang.annotation.Retention
import java.lang.annotation.RetentionPolicy
import java.lang.annotation.Target

@Target([ElementType.METHOD, ElementType.TYPE])
@Retention(RetentionPolicy.RUNTIME)
@EnabledIfSystemProperty(named = 'z390.test.mode', matches = 'full')
@interface OptionalTest {
}