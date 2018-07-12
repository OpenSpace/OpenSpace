import React from 'react';
import PropTypes from 'prop-types';
import styles from './FlexColumn.scss';

/**
 * Parent of this component needs display: flex;
 */
const FlexColumn = (props) => {
    const { widthPercentage } = props;

    return (
        <div className={`${styles.column} ${props.className}`} 
            style={widthPercentage ? {width: `${widthPercentage}%`} : {}}>
            {props.children}
        </div>
    );
}

FlexColumn.propTypes = {
    widthPercentage: PropTypes.number,
    className: PropTypes.string
}

FlexColumn.defaultProps = {
    widthPercentage: null,
    className: ''
}

export default FlexColumn
